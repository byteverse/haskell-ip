{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

{-| This module provides the Mac data type and functions for working
    with it.
-}
module Net.Mac
  ( -- * Convert
    mac
  , fromOctets
  , toOctets
    -- * Textual Conversion
    -- ** Text
  , encode
  , encodeWith
  , decode
  , decodeWith
  , builder
  , parser
  , parserWith
    -- ** UTF-8 ByteString
  , encodeUtf8
  , encodeWithUtf8
  , decodeUtf8
  , decodeWithUtf8
  , builderUtf8
  , parserUtf8
  , parserWithUtf8
    -- ** ByteString
  , decodeBytes
    -- ** Printing
  , print
    -- * Types
  , Mac(..)
  , MacCodec(..)
  , MacGrouping(..)
  ) where

import Prelude hiding (print)

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON(..),ToJSON(..))
import Data.Aeson (ToJSONKey(..),FromJSONKey(..))
import Data.Aeson (ToJSONKeyFunction(..),FromJSONKeyFunction(..))
import Data.Bits ((.|.),unsafeShiftL,unsafeShiftR,(.&.))
import Data.ByteString (ByteString)
import Data.Char (ord,chr)
import Data.Hashable (Hashable)
import Data.Primitive.Types (Prim(..))
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif
import Data.Text (Text)
import Data.Word
import Data.Word.Synthetic.Word12 (Word12)
import GHC.Enum (predError, succError)
import GHC.Exts
import GHC.Generics (Generic)
import GHC.Word (Word16(W16#))
import Text.ParserCombinators.ReadPrec (prec,step)
import Text.Read (Read(..),Lexeme(Ident),lexP,parens)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString as ABW
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Fixed as BFB
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Text.Builder.Fixed as TFB
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text as Text ()

-- $setup
--
-- These are here to get doctest's property checking to work
--
-- >>> :set -XOverloadedStrings
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> import qualified Data.Text as Text (pack)
-- >>> import qualified Data.Text.IO as T
-- >>> import qualified Data.ByteString.Char8 as BC
-- >>> instance Arbitrary Mac where { arbitrary = fmap (Mac . (0xFFFFFFFFFFFF .&.)) arbitrary }

-- | Construct a 'Mac' address from a 'Word64'. Only the lower
--   48 bits are used.
mac :: Word64 -> Mac
mac w = Mac (w .&. 0xFFFFFFFFFFFF)

-- | Create a 'Mac' address from six octets.
fromOctets :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Mac
fromOctets a b c d e f = Mac $ unsafeWord48FromOctets
  (fromIntegral a) (fromIntegral b) (fromIntegral c)
  (fromIntegral d) (fromIntegral e) (fromIntegral f)

-- | Convert a 'Mac' address to the six octets that make it up.
--   This function and 'fromOctets' are inverses:
--
--   prop> m == (let (a,b,c,d,e,f) = toOctets m in fromOctets a b c d e f)
toOctets :: Mac -> (Word8,Word8,Word8,Word8,Word8,Word8)
toOctets (Mac w) =
  ( fromIntegral $ unsafeShiftR w 40
  , fromIntegral $ unsafeShiftR w 32
  , fromIntegral $ unsafeShiftR w 24
  , fromIntegral $ unsafeShiftR w 16
  , fromIntegral $ unsafeShiftR w 8
  , fromIntegral w
  )

-- | Decode a 'Mac' address from a 'ByteString'. Each byte is interpreted
--   as an octet of the 'Mac' address. Consequently, 'ByteString's
--   of length 6 successfully decode, and all other 'ByteString's fail
--   to decode.
--
--   >>> decodeBytes (B.pack [0x6B,0x47,0x18,0x90,0x55,0xC3])
--   Just (mac 0x6b47189055c3)
--   >>> decodeBytes (B.replicate 6 0x3A)
--   Just (mac 0x3a3a3a3a3a3a)
--   >>> decodeBytes (B.replicate 7 0x3A)
--   Nothing
decodeBytes :: ByteString -> Maybe Mac
decodeBytes bs = if B.length bs == 6
  then Just $ fromOctets
    (BU.unsafeIndex bs 0)
    (BU.unsafeIndex bs 1)
    (BU.unsafeIndex bs 2)
    (BU.unsafeIndex bs 3)
    (BU.unsafeIndex bs 4)
    (BU.unsafeIndex bs 5)
  else Nothing

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

c2w :: Char -> Word8
c2w = fromIntegral . ord

-- | Encode a 'Mac' address using the default 'MacCodec' 'defCodec'.
--
--   >>> T.putStrLn (encode (Mac 0xA47F247AB423))
--   a4:7f:24:7a:b4:23
encode :: Mac -> Text
encode = encodeWith defCodec

-- | Encode a 'Mac' address using the given 'MacCodec'.
--
--   >>> m = Mac 0xA47F247AB423
--   >>> T.putStrLn $ encodeWith defCodec m
--   a4:7f:24:7a:b4:23
--
--   >>> T.putStrLn $ encodeWith (MacCodec (MacGroupingTriples '-') True) m
--   A47-F24-7AB-423
encodeWith :: MacCodec -> Mac -> Text
encodeWith (MacCodec g u) m = case g of
  MacGroupingNoSeparator -> case u of
    True -> TFB.run (fixedBuilderNoSeparator TFB.word8HexFixedUpper) m
    False -> TFB.run (fixedBuilderNoSeparator TFB.word8HexFixedLower) m
  MacGroupingPairs c -> case u of
    True -> TFB.run (fixedBuilderPairs TFB.word8HexFixedUpper) (Pair c m)
    False -> TFB.run (fixedBuilderPairs TFB.word8HexFixedLower) (Pair c m)
    -- withCasedBuilder u $ \bw8 -> TFB.run (fixedBuilderPairs bw8) (Pair c m)
  MacGroupingTriples c -> case u of
    True -> TFB.run (fixedBuilderTriples TFB.word12HexFixedUpper) (Pair c m)
    False -> TFB.run (fixedBuilderTriples TFB.word12HexFixedLower) (Pair c m)
  MacGroupingQuadruples c -> case u of
    True -> TFB.run (fixedBuilderQuadruples TFB.word8HexFixedUpper) (Pair c m)
    False -> TFB.run (fixedBuilderQuadruples TFB.word8HexFixedLower) (Pair c m)

-- | Decode a 'Mac' address using the default 'MacCodec' 'defCodec'.
--
--   >>> decode (Text.pack "a4:7f:24:7a:b4:23")
--   Just (mac 0xa47f247ab423)
--
--   >>> decode (Text.pack "a47-f24-7ab-423")
--   Nothing
decode :: Text -> Maybe Mac
decode = decodeWith defCodec

-- | Decode a 'Mac' address from 'Text' using the given 'MacCodec'.
--
-- >>> decodeWith defCodec (Text.pack "a4:7f:24:7a:b4:23")
-- Just (mac 0xa47f247ab423)
--
-- >>> decodeWith (MacCodec MacGroupingNoSeparator False) (Text.pack "a47f247ab423")
-- Just (mac 0xa47f247ab423)
decodeWith :: MacCodec -> Text -> Maybe Mac
decodeWith codec t = rightToMaybe (AT.parseOnly (parserWith codec <* AT.endOfInput) t)

-- | Encode a 'Mac' address as a 'TBuilder.Builder'.
builder :: Mac -> TBuilder.Builder
builder = TBuilder.fromText . encode

-- | Parse a 'Mac' address using a 'AT.Parser'.
--
--   >>> AT.parseOnly parser (Text.pack "a4:7f:24:7a:b4:23")
--   Right (mac 0xa47f247ab423)
--
--   >>> AT.parseOnly parser (Text.pack "a47-f24-7ab-423")
--   Left "':': Failed reading: satisfy"
parser :: AT.Parser Mac
parser = parserWith defCodec

-- | Parser a 'Mac' address using the given 'MacCodec'.
--
--   >>> p1 = parserWith defCodec
--   >>> AT.parseOnly p1 (Text.pack "a4:7f:24:7a:b4:23")
--   Right (mac 0xa47f247ab423)
--
--   >>> p2 = parserWith (MacCodec MacGroupingNoSeparator False)
--   >>> AT.parseOnly p2 (Text.pack "a47f247ab423")
--   Right (mac 0xa47f247ab423)
parserWith :: MacCodec -> AT.Parser Mac
parserWith (MacCodec g _) = case g of
  MacGroupingQuadruples c -> parserQuadruples c
  MacGroupingTriples c -> parserTriples c
  MacGroupingPairs c -> parserPairs c
  MacGroupingNoSeparator -> parserNoSeparator

-- | The default 'MacCodec': all characters are lowercase hex, separated by colons into pairs.
--
--   >>> T.putStrLn $ encodeWith defCodec (Mac 0xa47f247ab423)
--   a4:7f:24:7a:b4:23
defCodec :: MacCodec
defCodec = MacCodec (MacGroupingPairs ':') False

parserQuadruples :: Char -> AT.Parser Mac
parserQuadruples s = fromOctets
  <$> parseTwoHex <*> parseTwoHex <* AT.char s
  <*> parseTwoHex <*> parseTwoHex <* AT.char s
  <*> parseTwoHex <*> parseTwoHex

parserPairs :: Char -> AT.Parser Mac
parserPairs s = fromOctets
  <$> parseTwoHex <* AT.char s
  <*> parseTwoHex <* AT.char s
  <*> parseTwoHex <* AT.char s
  <*> parseTwoHex <* AT.char s
  <*> parseTwoHex <* AT.char s
  <*> parseTwoHex

parserTriples :: Char -> AT.Parser Mac
parserTriples s = do
  a1 <- parseOneHex
  a2 <- parseOneHex
  a3 <- parseOneHex
  _ <- AT.char s
  a4 <- parseOneHex
  a5 <- parseOneHex
  a6 <- parseOneHex
  _ <- AT.char s
  a7 <- parseOneHex
  a8 <- parseOneHex
  a9 <- parseOneHex
  _ <- AT.char s
  a10 <- parseOneHex
  a11 <- parseOneHex
  a12 <- parseOneHex
  return $ fromOctets
    (unsafeShiftL a1 4 + a2)
    (unsafeShiftL a3 4 + a4)
    (unsafeShiftL a5 4 + a6)
    (unsafeShiftL a7 4 + a8)
    (unsafeShiftL a9 4 + a10)
    (unsafeShiftL a11 4 + a12)

parserNoSeparator :: AT.Parser Mac
parserNoSeparator = fromOctets
  <$> parseTwoHex
  <*> parseTwoHex
  <*> parseTwoHex
  <*> parseTwoHex
  <*> parseTwoHex
  <*> parseTwoHex

parseTwoHex :: AT.Parser Word8
parseTwoHex = do
  a <- AT.anyChar >>= parseCharHex
  b <- AT.anyChar >>= parseCharHex
  return (unsafeShiftL a 4 + b)

tryParseCharHex :: AT.Parser Word8 -> Char -> AT.Parser Word8
tryParseCharHex a c
  | w >= 48 && w <= 57 = return (w - 48)
  | w >= 65 && w <= 70 = return (w - 55)
  | w >= 97 && w <= 102 = return (w - 87)
  | otherwise = a
  where w = c2w c

parseOneHex :: AT.Parser Word8
parseOneHex = AT.anyChar >>= parseCharHex

parseCharHex :: Char -> AT.Parser Word8
parseCharHex = tryParseCharHex (fail "invalid hexadecimal character")

data Pair = Pair
  { pairSep :: {-# UNPACK #-} !Char
  , pairMac :: {-# UNPACK #-} !Mac
  }

fixedBuilderTriples :: TFB.Builder Word12 -> TFB.Builder Pair
fixedBuilderTriples tripBuilder =
     TFB.contramapBuilder (word12At 36 . pairMac) tripBuilder
  <> TFB.contramapBuilder pairSep TFB.charBmp
  <> TFB.contramapBuilder (word12At 24 . pairMac) tripBuilder
  <> TFB.contramapBuilder pairSep TFB.charBmp
  <> TFB.contramapBuilder (word12At 12 . pairMac) tripBuilder
  <> TFB.contramapBuilder pairSep TFB.charBmp
  <> TFB.contramapBuilder (word12At 0 . pairMac) tripBuilder
{-# INLINE fixedBuilderTriples #-}

fixedBuilderNoSeparator :: TFB.Builder Word8 -> TFB.Builder Mac
fixedBuilderNoSeparator hexBuilder =
     TFB.contramapBuilder (word8At 40) hexBuilder
  <> TFB.contramapBuilder (word8At 32) hexBuilder
  <> TFB.contramapBuilder (word8At 24) hexBuilder
  <> TFB.contramapBuilder (word8At 16) hexBuilder
  <> TFB.contramapBuilder (word8At 8) hexBuilder
  <> TFB.contramapBuilder (word8At 0) hexBuilder
{-# INLINE fixedBuilderNoSeparator #-}

fixedBuilderQuadruples :: TFB.Builder Word8 -> TFB.Builder Pair
fixedBuilderQuadruples pairBuilder =
     TFB.contramapBuilder (word8At 40 . pairMac) pairBuilder
  <> TFB.contramapBuilder (word8At 32 . pairMac) pairBuilder
  <> TFB.contramapBuilder pairSep TFB.charBmp
  <> TFB.contramapBuilder (word8At 24 . pairMac) pairBuilder
  <> TFB.contramapBuilder (word8At 16 . pairMac) pairBuilder
  <> TFB.contramapBuilder pairSep TFB.charBmp
  <> TFB.contramapBuilder (word8At 8 . pairMac) pairBuilder
  <> TFB.contramapBuilder (word8At 0 . pairMac) pairBuilder
{-# INLINE fixedBuilderQuadruples #-}

fixedBuilderPairs :: TFB.Builder Word8 -> TFB.Builder Pair
fixedBuilderPairs pairBuilder =
     TFB.contramapBuilder (word8At 40 . pairMac) pairBuilder
  <> TFB.contramapBuilder pairSep TFB.charBmp
  <> TFB.contramapBuilder (word8At 32 . pairMac) pairBuilder
  <> TFB.contramapBuilder pairSep TFB.charBmp
  <> TFB.contramapBuilder (word8At 24 . pairMac) pairBuilder
  <> TFB.contramapBuilder pairSep TFB.charBmp
  <> TFB.contramapBuilder (word8At 16 . pairMac) pairBuilder
  <> TFB.contramapBuilder pairSep TFB.charBmp
  <> TFB.contramapBuilder (word8At 8 . pairMac) pairBuilder
  <> TFB.contramapBuilder pairSep TFB.charBmp
  <> TFB.contramapBuilder (word8At 0 . pairMac) pairBuilder
{-# INLINE fixedBuilderPairs #-}

word8At :: Int -> Mac -> Word8
word8At i (Mac w) = fromIntegral (unsafeShiftR w i)
{-# INLINE word8At #-}

word12At :: Int -> Mac -> Word12
word12At i (Mac w) = fromIntegral (unsafeShiftR w i)
{-# INLINE word12At #-}

-- | Encode a 'Mac' address using the default 'MacCodec' 'defCodec'.
--
--   >>> BC.putStrLn (encodeUtf8 (mac 0x64255A0F2C47))
--   64:25:5a:0f:2c:47
encodeUtf8 :: Mac -> ByteString
encodeUtf8 = encodeWithUtf8 defCodec

-- | Lenient decoding of MAC address that accepts lowercase, uppercase,
--   and any kind of separator.
--
--   >>> decodeUtf8 "A2:DE:AD:BE:EF:67"
--   Just (mac 0xa2deadbeef67)
--   >>> decodeUtf8 "13-a2-fe-a4-17-96"
--   Just (mac 0x13a2fea41796)
--   >>> decodeUtf8 "0A42.47BA.67C2"
--   Just (mac 0x0a4247ba67c2)
decodeUtf8 :: ByteString -> Maybe Mac
decodeUtf8 = decodeLenientUtf8

-- | Decode a 'ByteString' as a 'Mac' address using the given 'MacCodec'.
--
--   >>> decodeWithUtf8 defCodec (BC.pack "64:25:5a:0f:2c:47")
--   Just (mac 0x64255a0f2c47)
--
--   >>> decodeWithUtf8 (MacCodec MacGroupingNoSeparator False) (BC.pack "64255a0f2c47")
--   Just (mac 0x64255a0f2c47)
decodeWithUtf8 :: MacCodec -> ByteString -> Maybe Mac
decodeWithUtf8 codec bs = rightToMaybe (AB.parseOnly (parserWithUtf8 codec <* AB.endOfInput) bs)

decodeLenientUtf8 :: ByteString -> Maybe Mac
decodeLenientUtf8 bs = rightToMaybe (AB.parseOnly (parserLenientUtf8 <* AB.endOfInput) bs)

-- | Make a bytestring builder from a 'Mac' address
--   using a colon as the separator.
builderUtf8 :: Mac -> BB.Builder
builderUtf8 = BB.byteString . encodeUtf8

-- | Lenient parser for a 'Mac' address using any character
--   as the separator and accepting any digit grouping
--   (i.e. @FA:43:B2:C0:0F:99@ or @A065.647B.87FA@).
parserUtf8 :: AB.Parser Mac
parserUtf8 = parserLenientUtf8

-- | Parser for a 'Mac' address using the provided settings.
parserWithUtf8 :: MacCodec -> AB.Parser Mac
parserWithUtf8 (MacCodec g _) = case g of
  MacGroupingPairs s -> parserPairsUtf8 (c2w s)
  MacGroupingTriples s -> parserTriplesUtf8 (c2w s)
  MacGroupingQuadruples s -> parserQuadruplesUtf8 (c2w s)
  MacGroupingNoSeparator -> parserNoSeparatorUtf8

parserLenientUtf8 :: AB.Parser Mac
parserLenientUtf8 = do
  a1 <- parseOneHexUtf8
  a2 <- parseOneHexLenientUtf8
  a3 <- parseOneHexLenientUtf8
  a4 <- parseOneHexLenientUtf8
  a5 <- parseOneHexLenientUtf8
  a6 <- parseOneHexLenientUtf8
  a7 <- parseOneHexLenientUtf8
  a8 <- parseOneHexLenientUtf8
  a9 <- parseOneHexLenientUtf8
  a10 <- parseOneHexLenientUtf8
  a11 <- parseOneHexLenientUtf8
  a12 <- parseOneHexLenientUtf8
  return $ fromOctets
    (unsafeShiftL a1 4 + a2)
    (unsafeShiftL a3 4 + a4)
    (unsafeShiftL a5 4 + a6)
    (unsafeShiftL a7 4 + a8)
    (unsafeShiftL a9 4 + a10)
    (unsafeShiftL a11 4 + a12)


parserNoSeparatorUtf8 :: AB.Parser Mac
parserNoSeparatorUtf8 = fromOctets
  <$> parseTwoHexUtf8
  <*> parseTwoHexUtf8
  <*> parseTwoHexUtf8
  <*> parseTwoHexUtf8
  <*> parseTwoHexUtf8
  <*> parseTwoHexUtf8

parserPairsUtf8 :: Word8 -> AB.Parser Mac
parserPairsUtf8 s = fromOctets
  <$> parseTwoHexUtf8 <* ABW.word8 s
  <*> parseTwoHexUtf8 <* ABW.word8 s
  <*> parseTwoHexUtf8 <* ABW.word8 s
  <*> parseTwoHexUtf8 <* ABW.word8 s
  <*> parseTwoHexUtf8 <* ABW.word8 s
  <*> parseTwoHexUtf8

parserTriplesUtf8 :: Word8 -> AB.Parser Mac
parserTriplesUtf8 s = do
  a1 <- parseOneHexUtf8
  a2 <- parseOneHexUtf8
  a3 <- parseOneHexUtf8
  _ <- ABW.word8 s
  a4 <- parseOneHexUtf8
  a5 <- parseOneHexUtf8
  a6 <- parseOneHexUtf8
  _ <- ABW.word8 s
  a7 <- parseOneHexUtf8
  a8 <- parseOneHexUtf8
  a9 <- parseOneHexUtf8
  _ <- ABW.word8 s
  a10 <- parseOneHexUtf8
  a11 <- parseOneHexUtf8
  a12 <- parseOneHexUtf8
  return $ fromOctets
    (unsafeShiftL a1 4 + a2)
    (unsafeShiftL a3 4 + a4)
    (unsafeShiftL a5 4 + a6)
    (unsafeShiftL a7 4 + a8)
    (unsafeShiftL a9 4 + a10)
    (unsafeShiftL a11 4 + a12)

parserQuadruplesUtf8 :: Word8 -> AB.Parser Mac
parserQuadruplesUtf8 s  = fromOctets
  <$> parseTwoHexUtf8 <*> parseTwoHexUtf8 <* ABW.word8 s
  <*> parseTwoHexUtf8 <*> parseTwoHexUtf8 <* ABW.word8 s
  <*> parseTwoHexUtf8 <*> parseTwoHexUtf8

parseOneHexUtf8 :: AB.Parser Word8
parseOneHexUtf8 = ABW.anyWord8 >>= parseWord8Hex

-- | Parse a single hexidecimal character. This will skip
--   at most one character to do this.
parseOneHexLenientUtf8 :: AB.Parser Word8
parseOneHexLenientUtf8 = do
  a <- ABW.anyWord8
  flip tryParseWord8Hex a $ do
    b <- ABW.anyWord8
    tryParseWord8Hex (fail "invalid hexadecimal character") b

parseTwoHexUtf8 :: AB.Parser Word8
parseTwoHexUtf8 = do
  a <- ABW.anyWord8 >>= parseWord8Hex
  b <- ABW.anyWord8 >>= parseWord8Hex
  return (unsafeShiftL a 4 + b)

-- | Kind of a confusing type signature. The Word8 that stands
--   alone represented an ascii-encoded value. The others actually
--   describes the numbers that would be decoded from this value.
tryParseWord8Hex :: AB.Parser Word8 -> Word8 -> AB.Parser Word8
tryParseWord8Hex a w
  | w >= 48 && w <= 57 = return (w - 48)
  | w >= 65 && w <= 70 = return (w - 55)
  | w >= 97 && w <= 102 = return (w - 87)
  | otherwise = a

parseWord8Hex :: Word8 -> AB.Parser Word8
parseWord8Hex = tryParseWord8Hex (fail "invalid hexadecimal character")

-- | Encode a 'Mac' address as a 'ByteString' using the given 'MacCodec'.
--
--   >>> m = Mac 0xA47F247AB423
--   >>> BC.putStrLn $ encodeWithUtf8 defCodec m
--   a4:7f:24:7a:b4:23
--
--   >>> BC.putStrLn $ encodeWithUtf8 (MacCodec (MacGroupingTriples '-') True) m
--   A47-F24-7AB-423
encodeWithUtf8 :: MacCodec -> Mac -> ByteString
encodeWithUtf8 (MacCodec g u) m = case g of
  MacGroupingNoSeparator -> case u of
    True -> BFB.run (fixedBuilderNoSeparatorUtf8 BFB.word8HexFixedUpper) m
    False -> BFB.run (fixedBuilderNoSeparatorUtf8 BFB.word8HexFixedLower) m
  MacGroupingPairs c -> case u of
    True -> BFB.run (fixedBuilderPairsUtf8 BFB.word8HexFixedUpper) (PairUtf8 (c2w c) m)
    False -> BFB.run (fixedBuilderPairsUtf8 BFB.word8HexFixedLower) (PairUtf8 (c2w c) m)
    -- withCasedBuilder u $ \bw8 -> BFB.run (fixedBuilderPairs bw8) (Pair c m)
  MacGroupingTriples c -> case u of
    True -> BFB.run (fixedBuilderTriplesUtf8 BFB.word12HexFixedUpper) (PairUtf8 (c2w c) m)
    False -> BFB.run (fixedBuilderTriplesUtf8 BFB.word12HexFixedLower) (PairUtf8 (c2w c) m)
  MacGroupingQuadruples c -> case u of
    True -> BFB.run (fixedBuilderQuadruplesUtf8 BFB.word8HexFixedUpper) (PairUtf8 (c2w c) m)
    False -> BFB.run (fixedBuilderQuadruplesUtf8 BFB.word8HexFixedLower) (PairUtf8 (c2w c) m)

data PairUtf8 = PairUtf8
  { pairSepUtf8 :: {-# UNPACK #-} !Word8
  , pairMacUtf8 :: {-# UNPACK #-} !Mac
  }

fixedBuilderTriplesUtf8 :: BFB.Builder Word12 -> BFB.Builder PairUtf8
fixedBuilderTriplesUtf8 tripBuilder =
     BFB.contramapBuilder (word12AtUtf8 36 . pairMacUtf8) tripBuilder
  <> BFB.contramapBuilder pairSepUtf8 BFB.word8
  <> BFB.contramapBuilder (word12AtUtf8 24 . pairMacUtf8) tripBuilder
  <> BFB.contramapBuilder pairSepUtf8 BFB.word8
  <> BFB.contramapBuilder (word12AtUtf8 12 . pairMacUtf8) tripBuilder
  <> BFB.contramapBuilder pairSepUtf8 BFB.word8
  <> BFB.contramapBuilder (word12AtUtf8 0 . pairMacUtf8) tripBuilder
{-# INLINE fixedBuilderTriplesUtf8 #-}

fixedBuilderQuadruplesUtf8 :: BFB.Builder Word8 -> BFB.Builder PairUtf8
fixedBuilderQuadruplesUtf8 pairBuilder =
     BFB.contramapBuilder (word8AtUtf8 40 . pairMacUtf8) pairBuilder
  <> BFB.contramapBuilder (word8AtUtf8 32 . pairMacUtf8) pairBuilder
  <> BFB.contramapBuilder pairSepUtf8 BFB.word8
  <> BFB.contramapBuilder (word8AtUtf8 24 . pairMacUtf8) pairBuilder
  <> BFB.contramapBuilder (word8AtUtf8 16 . pairMacUtf8) pairBuilder
  <> BFB.contramapBuilder pairSepUtf8 BFB.word8
  <> BFB.contramapBuilder (word8AtUtf8 8 . pairMacUtf8) pairBuilder
  <> BFB.contramapBuilder (word8AtUtf8 0 . pairMacUtf8) pairBuilder
{-# INLINE fixedBuilderQuadruplesUtf8 #-}

fixedBuilderPairsUtf8 :: BFB.Builder Word8 -> BFB.Builder PairUtf8
fixedBuilderPairsUtf8 pairBuilder =
     BFB.contramapBuilder (word8AtUtf8 40 . pairMacUtf8) pairBuilder
  <> BFB.contramapBuilder pairSepUtf8 BFB.word8
  <> BFB.contramapBuilder (word8AtUtf8 32 . pairMacUtf8) pairBuilder
  <> BFB.contramapBuilder pairSepUtf8 BFB.word8
  <> BFB.contramapBuilder (word8AtUtf8 24 . pairMacUtf8) pairBuilder
  <> BFB.contramapBuilder pairSepUtf8 BFB.word8
  <> BFB.contramapBuilder (word8AtUtf8 16 . pairMacUtf8) pairBuilder
  <> BFB.contramapBuilder pairSepUtf8 BFB.word8
  <> BFB.contramapBuilder (word8AtUtf8 8 . pairMacUtf8) pairBuilder
  <> BFB.contramapBuilder pairSepUtf8 BFB.word8
  <> BFB.contramapBuilder (word8AtUtf8 0 . pairMacUtf8) pairBuilder
{-# INLINE fixedBuilderPairsUtf8 #-}

fixedBuilderNoSeparatorUtf8 :: BFB.Builder Word8 -> BFB.Builder Mac
fixedBuilderNoSeparatorUtf8 hexBuilder =
     BFB.contramapBuilder (word8AtUtf8 40) hexBuilder
  <> BFB.contramapBuilder (word8AtUtf8 32) hexBuilder
  <> BFB.contramapBuilder (word8AtUtf8 24) hexBuilder
  <> BFB.contramapBuilder (word8AtUtf8 16) hexBuilder
  <> BFB.contramapBuilder (word8AtUtf8 8) hexBuilder
  <> BFB.contramapBuilder (word8AtUtf8 0) hexBuilder
{-# INLINE fixedBuilderNoSeparatorUtf8 #-}

word8AtUtf8 :: Int -> Mac -> Word8
word8AtUtf8 i (Mac w) = fromIntegral (unsafeShiftR w i)
{-# INLINE word8AtUtf8 #-}

word12AtUtf8 :: Int -> Mac -> Word12
word12AtUtf8 i (Mac w) = fromIntegral (unsafeShiftR w i)
{-# INLINE word12AtUtf8 #-}

-- | A 48-bit MAC address. Do not use the data constructor for this
--   type. It is not considered part of the stable API, and it
--   allows you to construct invalid MAC addresses.
newtype Mac = Mac Word64
  deriving (Eq,Ord,Generic)

instance NFData Mac

-- | This only preserves the lower 6 bytes of the 8-byte word that backs a mac address.
-- It runs slower than it would if it used a full 8-byte word, but it consumes less
-- space. When storing millions of mac addresses, this is a good trade to make. When
-- storing a small number of mac address, it might be preferable to make a primitive
-- array of 'Word64' instead and use the mac address data constructor to coerce between
-- the two.
instance Prim Mac where
  sizeOf# _ = 6#
  alignment# _ = 2#
  indexByteArray# arr i0 = macFromWord16#
    (indexWord16Array# arr i)
    (indexWord16Array# arr (i +# 1#))
    (indexWord16Array# arr (i +# 2#))
    where !i = 3# *# i0
  readByteArray# arr i0 s0 = case readWord16Array# arr i s0 of
    (# s1, a #) -> case readWord16Array# arr (i +# 1#) s1 of
      (# s2, b #) -> case readWord16Array# arr (i +# 2#) s2 of
        (# s3, c #) -> (# s3, macFromWord16# a b c #)
    where !i = 3# *# i0
  writeByteArray# arr i0 m s0 = case writeWord16Array# arr i (macToWord16A# m) s0 of
    s1 -> case writeWord16Array# arr (i +# 1#) (macToWord16B# m) s1 of
      s2 -> writeWord16Array# arr (i +# 2#) (macToWord16C# m) s2
    where !i = 3# *# i0
  indexOffAddr# arr i0 = macFromWord16#
    (indexWord16OffAddr# arr i)
    (indexWord16OffAddr# arr (i +# 1#))
    (indexWord16OffAddr# arr (i +# 2#))
    where !i = 3# *# i0
  readOffAddr# arr i0 s0 = case readWord16OffAddr# arr i s0 of
    (# s1, a #) -> case readWord16OffAddr# arr (i +# 1#) s1 of
      (# s2, b #) -> case readWord16OffAddr# arr (i +# 2#) s2 of
        (# s3, c #) -> (# s3, macFromWord16# a b c #)
    where !i = 3# *# i0
  writeOffAddr# arr i0 m s0 = case writeWord16OffAddr# arr i (macToWord16A# m) s0 of
    s1 -> case writeWord16OffAddr# arr (i +# 1#) (macToWord16B# m) s1 of
      s2 -> writeWord16OffAddr# arr (i +# 2#) (macToWord16C# m) s2
    where !i = 3# *# i0
  setByteArray# arr# i# len# ident = go 0#
    where
      go ix# s0 = if isTrue# (ix# <# len#)
        then case writeByteArray# arr# (i# +# ix#) ident s0 of
          s1 -> go (ix# +# 1#) s1
        else s0
  setOffAddr# addr# i# len# ident = go 0#
    where
      go ix# s0 = if isTrue# (ix# <# len#)
        then case writeOffAddr# addr# (i# +# ix#) ident s0 of
          s1 -> go (ix# +# 1#) s1
        else s0

macToWord16A# :: Mac -> Word#
macToWord16A# (Mac w) = case word64ToWord16 (unsafeShiftR w 32) of
  W16# x -> x
  
macToWord16B# :: Mac -> Word#
macToWord16B# (Mac w) = case word64ToWord16 (unsafeShiftR w 16) of
  W16# x -> x

macToWord16C# :: Mac -> Word#
macToWord16C# (Mac w) = case word64ToWord16 w of
  W16# x -> x
  
macFromWord16# :: Word# -> Word# -> Word# -> Mac
macFromWord16# a b c = Mac
    $ (unsafeShiftL (word16ToWord64 (W16# a)) 32)
  .|. (unsafeShiftL (word16ToWord64 (W16# b)) 16)
  .|. (word16ToWord64 (W16# c))

word16ToWord64 :: Word16 -> Word64
word16ToWord64 = fromIntegral

word64ToWord16 :: Word64 -> Word16
word64ToWord16 = fromIntegral

-- What this instance does is to display the
-- inner contents in hexadecimal and pad them out to
-- 6 bytes in case it begins with several zeroes.
-- It also uses the smart constructor instead
-- of the actual constructor
instance Show Mac where
  showsPrec p (Mac addr) = showParen (p > 10)
    $ showString "mac "
    . showHexWord48 addr

instance Read Mac where
  readPrec = parens $ prec 10 $ do
    Ident "mac" <- lexP
    w <- step readPrec
    return (mac w)

instance Bounded Mac where
  minBound = Mac 0
  maxBound = Mac 0xFFFFFFFFFFFF

instance Enum Mac where
  succ m@(Mac x)
    | m == maxBound = succError "Mac"
    | otherwise = Mac (x + 1)
  pred m@(Mac x)
    | m == minBound = predError "Mac"
    | otherwise = Mac (x - 1)
  toEnum i = Mac (toEnum i)
  fromEnum (Mac x) = fromEnum x

-- | Print a 'Mac' address using the textual encoding.
print :: Mac -> IO ()
print = TIO.putStrLn . encode

showHexWord48 :: Word64 -> ShowS
showHexWord48 w = showString "0x" . go 11
  where
  go :: Int -> ShowS
  go !ix = if ix >= 0
    then showChar (nibbleToHex ((unsafeShiftR (fromIntegral w) (ix * 4)) .&. 0xF)) . go (ix - 1)
    else id

nibbleToHex :: Word -> Char
nibbleToHex w
  | w < 10 = chr (fromIntegral (w + 48))
  | otherwise = chr (fromIntegral (w + 87))

-- | A 'MacCodec' allows users to control the encoding/decoding
--   of their 'Mac' addresses.
data MacCodec = MacCodec
  { macCodecGrouping :: !MacGrouping
  , macCodecUpperCase :: !Bool
  } deriving (Eq,Ord,Show,Read,Generic)

-- | The format expected by the mac address parser. The 'Word8' taken
--   by some of these constructors is the ascii value of the character
--   to be used as the separator. This is typically a colon, a hyphen, or
--   a space character. All decoding functions are case insensitive.
data MacGrouping
  = MacGroupingPairs !Char -- ^ Two-character groups, @FA:2B:40:09:8C:11@
  | MacGroupingTriples !Char -- ^ Three-character groups, @24B-F0A-025-829@
  | MacGroupingQuadruples !Char -- ^ Four-character groups, @A220.0745.CAC7@
  | MacGroupingNoSeparator -- ^ No separator, @24AF4B5B0780@
  deriving (Eq,Ord,Show,Read,Generic)

instance Hashable Mac

instance ToJSON Mac where
  toJSON = Aeson.String . encode

instance ToJSONKey Mac where
  toJSONKey = ToJSONKeyText
    encode
    (\m -> Aeson.unsafeToEncoding $ BB.char7 '"' <> builderUtf8 m <> BB.char7 '"')

instance FromJSONKey Mac where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case decode t of
    Nothing -> fail "invalid mac address"
    Just addr -> return addr

instance FromJSON Mac where
  parseJSON = attoparsecParseJSON parser

attoparsecParseJSON :: AT.Parser a -> Aeson.Value -> Aeson.Parser a
attoparsecParseJSON p v =
  case v of
    Aeson.String t ->
      case AT.parseOnly p t of
        Left err  -> fail err
        Right res -> return res
    _ -> fail "expected a String"

-- Unchecked invariant: each of these Word64s must be smaller
-- than 256.
unsafeWord48FromOctets :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64
unsafeWord48FromOctets a b c d e f =
    fromIntegral
  $ unsafeShiftL a 40
  .|. unsafeShiftL b 32
  .|. unsafeShiftL c 24
  .|. unsafeShiftL d 16
  .|. unsafeShiftL e 8
  .|. f

