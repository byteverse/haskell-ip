{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wall #-}
module Net.Mac
  ( -- * Convert
    fromOctets
  , toOctets
    -- * Textual Conversion
    -- ** Text
  , encode
  , encodeWith
  , decode
  , decodeWith
  , decodeEither
  , decodeEitherWith
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
  , parserLenientUtf8
    -- * Types
  , Mac(..)
  , MacCodec(..)
  , MacGrouping(..)
  ) where

import Data.Word
import Data.Bits ((.|.),unsafeShiftL,unsafeShiftR,(.&.))
import Data.Text (Text)
import Data.Word (Word8)
import Data.Word.Synthetic.Word12 (Word12)
import Data.Monoid
import Data.ByteString (ByteString)
import Data.Aeson (FromJSON(..),ToJSON(..))
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.Char (ord,chr)
import qualified Data.ByteString.Builder as BB
import qualified Data.Attoparsec.ByteString as ABW
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Builder.Fixed as TFB
import qualified Data.ByteString.Builder.Fixed as BFB
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

#if MIN_VERSION_aeson(1,0,0) 
import Data.Aeson (ToJSONKey(..),FromJSONKey(..),
  ToJSONKeyFunction(..),FromJSONKeyFunction(..))
#endif

fromOctets :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Mac
fromOctets a b c d e f = Mac $ unsafeWord48FromOctets
  (fromIntegral a) (fromIntegral b) (fromIntegral c)
  (fromIntegral d) (fromIntegral e) (fromIntegral f)
{-# INLINE fromOctets #-}

toOctets :: Mac -> (Word8,Word8,Word8,Word8,Word8,Word8)
toOctets (Mac w) =
  ( fromIntegral $ unsafeShiftR w 40
  , fromIntegral $ unsafeShiftR w 32
  , fromIntegral $ unsafeShiftR w 24
  , fromIntegral $ unsafeShiftR w 16
  , fromIntegral $ unsafeShiftR w 8
  , fromIntegral w
  )

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

c2w :: Char -> Word8
c2w = fromIntegral . ord

encode :: Mac -> Text
encode = encodeWith defCodec -- Internal.macToTextDefault w

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

decodeEitherWith :: MacCodec -> Text -> Either String Mac
decodeEitherWith codec t = AT.parseOnly (parserWith codec <* AT.endOfInput) t

decodeEither :: Text -> Either String Mac
decodeEither = decodeEitherWith defCodec

decode :: Text -> Maybe Mac
decode = decodeWith defCodec

decodeWith :: MacCodec -> Text -> Maybe Mac
decodeWith codec t = rightToMaybe (AT.parseOnly (parserWith codec <* AT.endOfInput) t)

builder :: Mac -> TBuilder.Builder
builder = TBuilder.fromText . encode

parser :: AT.Parser Mac
parser = parserWith defCodec

parserWith :: MacCodec -> AT.Parser Mac
parserWith (MacCodec g _) = case g of
  MacGroupingQuadruples c -> parserQuadruples c
  MacGroupingTriples c -> parserTriples c
  MacGroupingPairs c -> parserPairs c
  MacGroupingNoSeparator -> parserNoSeparator

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

encodeUtf8 :: Mac -> ByteString
encodeUtf8 = encodeWithUtf8 defCodec

-- | Lenient decoding of MAC address that accepts lowercase, uppercase,
--   and any kind separator.
decodeUtf8 :: ByteString -> Maybe Mac
decodeUtf8 = decodeLenientUtf8

decodeWithUtf8 :: MacCodec -> ByteString -> Maybe Mac
decodeWithUtf8 codec bs = rightToMaybe (AB.parseOnly (parserWithUtf8 codec <* AB.endOfInput) bs)

decodeLenientUtf8 :: ByteString -> Maybe Mac
decodeLenientUtf8 bs = rightToMaybe (AB.parseOnly (parserLenientUtf8 <* AB.endOfInput) bs)

-- | Make a bytestring builder from a 'Mac' address
--   using a colon as the separator.
builderUtf8 :: Mac -> BB.Builder
builderUtf8 = BB.byteString . encodeUtf8

-- | Parser for a 'Mac' address using with a colon as the
--   separator (i.e. @FA:43:B2:C0:0F:99@).
parserUtf8 :: AB.Parser Mac
parserUtf8 = parserWithUtf8 defCodec

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
  deriving (Eq,Ord,Read,Generic)

-- interestingly, the derived Read instance for Mac is
-- completely compatible with this custom Show instance.
-- The only thing this instance does is to display the
-- inner contents in hexadecimal and pad them out to
-- 6 bytes in case it begins with several zeroes.
instance Show Mac where
  showsPrec p (Mac addr) = showParen (p > 10)
    $ showString "Mac "
    . showHexWord48 addr

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

#if MIN_VERSION_aeson(1,0,0) 
instance ToJSONKey Mac where
  toJSONKey = ToJSONKeyText
    encode
    (\m -> Aeson.unsafeToEncoding $ BB.char7 '"' <> builderUtf8 m <> BB.char7 '"')

instance FromJSONKey Mac where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case decode t of
    Nothing -> fail "invalid mac address"
    Just mac -> return mac
#endif

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

