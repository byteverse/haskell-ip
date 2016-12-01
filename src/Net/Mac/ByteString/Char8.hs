module Net.Mac.ByteString.Char8
  ( encode
  , encodeWith
  , decode
  , decodeWith
  , decodeLenient
  , builder
  , parser
  , parserWith
  , parserLenient
  ) where

import Net.Types (Mac(..),MacCodec(..),MacGrouping(..))
import Net.Mac (fromOctets)
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString.Lazy.Builder (Builder)
import Net.Internal (rightToMaybe,c2w)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Data.Word (Word8)
import Data.Word.Synthetic (Word12)
import Data.Bits (unsafeShiftL,unsafeShiftR)
import Control.Monad
import Data.Monoid
import qualified Data.ByteString.Builder.Fixed as FB
import qualified Data.ByteString.Builder as Builder
import qualified Data.Attoparsec.ByteString as ABW
import qualified Data.Attoparsec.ByteString.Char8 as AB

encode :: Mac -> ByteString
encode = encodeWith defCodec

decode :: ByteString -> Maybe Mac
decode = decodeWith defCodec

decodeWith :: MacCodec -> ByteString -> Maybe Mac
decodeWith codec bs = rightToMaybe (AB.parseOnly (parserWith codec <* AB.endOfInput) bs)

decodeLenient :: ByteString -> Maybe Mac
decodeLenient bs = rightToMaybe (AB.parseOnly (parserLenient <* AB.endOfInput) bs)

-- | Make a bytestring builder from a 'Mac' address
--   using a colon as the separator.
builder :: Mac -> Builder
builder = Builder.byteString . encode

-- | Parser for a 'Mac' address using with a colon as the
--   separator (i.e. @FA:43:B2:C0:0F:99@).
parser :: Parser Mac
parser = parserWith defCodec

-- | Parser for a 'Mac' address using the provided settings.
parserWith :: MacCodec -> Parser Mac
parserWith (MacCodec g _) = case g of
  MacGroupingPairs s -> parserPairs (c2w s)
  MacGroupingTriples s -> parserTriples (c2w s)
  MacGroupingQuadruples s -> parserQuadruples (c2w s)
  MacGroupingNoSeparator -> parserNoSeparator

parserLenient :: Parser Mac
parserLenient = do
  a1 <- parseOneHex
  a2 <- parseOneHexLenient
  a3 <- parseOneHexLenient
  a4 <- parseOneHexLenient
  a5 <- parseOneHexLenient
  a6 <- parseOneHexLenient
  a7 <- parseOneHexLenient
  a8 <- parseOneHexLenient
  a9 <- parseOneHexLenient
  a10 <- parseOneHexLenient
  a11 <- parseOneHexLenient
  a12 <- parseOneHexLenient
  return $ fromOctets
    (unsafeShiftL a1 4 + a2)
    (unsafeShiftL a3 4 + a4)
    (unsafeShiftL a5 4 + a6)
    (unsafeShiftL a7 4 + a8)
    (unsafeShiftL a9 4 + a10)
    (unsafeShiftL a11 4 + a12)


parserNoSeparator :: Parser Mac
parserNoSeparator = fromOctets
  <$> parseTwoHex
  <*> parseTwoHex
  <*> parseTwoHex
  <*> parseTwoHex
  <*> parseTwoHex
  <*> parseTwoHex

parserPairs :: Word8 -> Parser Mac
parserPairs s = fromOctets
  <$> parseTwoHex <* ABW.word8 s
  <*> parseTwoHex <* ABW.word8 s
  <*> parseTwoHex <* ABW.word8 s
  <*> parseTwoHex <* ABW.word8 s
  <*> parseTwoHex <* ABW.word8 s
  <*> parseTwoHex

parserTriples :: Word8 -> Parser Mac
parserTriples s = do
  a1 <- parseOneHex
  a2 <- parseOneHex
  a3 <- parseOneHex
  _ <- ABW.word8 s
  a4 <- parseOneHex
  a5 <- parseOneHex
  a6 <- parseOneHex
  _ <- ABW.word8 s
  a7 <- parseOneHex
  a8 <- parseOneHex
  a9 <- parseOneHex
  _ <- ABW.word8 s
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

parserQuadruples :: Word8 -> Parser Mac
parserQuadruples s  = fromOctets
  <$> parseTwoHex <*> parseTwoHex <* ABW.word8 s
  <*> parseTwoHex <*> parseTwoHex <* ABW.word8 s
  <*> parseTwoHex <*> parseTwoHex

parseOneHex :: Parser Word8
parseOneHex = ABW.anyWord8 >>= parseWord8Hex

-- | Parse a single hexidecimal character. This will skip
--   at most one character to do this.
parseOneHexLenient :: Parser Word8
parseOneHexLenient = do
  a <- ABW.anyWord8
  flip tryParseWord8Hex a $ do
    b <- ABW.anyWord8
    tryParseWord8Hex (fail "invalid hexadecimal character") b

parseTwoHex :: Parser Word8
parseTwoHex = do
  a <- ABW.anyWord8 >>= parseWord8Hex
  b <- ABW.anyWord8 >>= parseWord8Hex
  return (unsafeShiftL a 4 + b)

-- | Kind of a confusing type signature. The Word8 that stands
--   alone represented an ascii-encoded value. The others actually
--   describes the numbers that would be decoded from this value.
tryParseWord8Hex :: Parser Word8 -> Word8 -> Parser Word8
tryParseWord8Hex a w
  | w >= 48 && w <= 57 = return (w - 48)
  | w >= 65 && w <= 70 = return (w - 55)
  | w >= 97 && w <= 102 = return (w - 87)
  | otherwise = a

parseWord8Hex :: Word8 -> Parser Word8
parseWord8Hex = tryParseWord8Hex (fail "invalid hexadecimal character")

defCodec :: MacCodec
defCodec = MacCodec (MacGroupingPairs ':') False

encodeWith :: MacCodec -> Mac -> ByteString
encodeWith (MacCodec g u) m = case g of
  MacGroupingNoSeparator -> case u of
    True -> FB.run (fixedBuilderNoSeparator FB.word8HexFixedUpper) m
    False -> FB.run (fixedBuilderNoSeparator FB.word8HexFixedLower) m
  MacGroupingPairs c -> case u of
    True -> FB.run (fixedBuilderPairs FB.word8HexFixedUpper) (Pair (c2w c) m)
    False -> FB.run (fixedBuilderPairs FB.word8HexFixedLower) (Pair (c2w c) m)
    -- withCasedBuilder u $ \bw8 -> FB.run (fixedBuilderPairs bw8) (Pair c m)
  MacGroupingTriples c -> case u of
    True -> FB.run (fixedBuilderTriples FB.word12HexFixedUpper) (Pair (c2w c) m)
    False -> FB.run (fixedBuilderTriples FB.word12HexFixedLower) (Pair (c2w c) m)
  MacGroupingQuadruples c -> case u of
    True -> FB.run (fixedBuilderQuadruples FB.word8HexFixedUpper) (Pair (c2w c) m)
    False -> FB.run (fixedBuilderQuadruples FB.word8HexFixedLower) (Pair (c2w c) m)

withCasedBuilder :: Bool -> (FB.Builder Word8 -> a) -> a
withCasedBuilder x f = case x of
  True -> f FB.word8HexFixedUpper
  False -> f FB.word8HexFixedLower
{-# INLINE withCasedBuilder #-}

withCasedBuilderTriple :: Bool -> (FB.Builder Word12 -> a) -> a
withCasedBuilderTriple x f = case x of
  True -> f FB.word12HexFixedUpper
  False -> f FB.word12HexFixedLower
{-# INLINE withCasedBuilderTriple #-}

data Pair = Pair
  { pairSep :: {-# UNPACK #-} !Word8
  , pairMac :: {-# UNPACK #-} !Mac
  }

fixedBuilderTriples :: FB.Builder Word12 -> FB.Builder Pair
fixedBuilderTriples tripBuilder =
     FB.contramapBuilder (word12At 36 . pairMac) tripBuilder
  <> FB.contramapBuilder pairSep FB.word8
  <> FB.contramapBuilder (word12At 24 . pairMac) tripBuilder
  <> FB.contramapBuilder pairSep FB.word8
  <> FB.contramapBuilder (word12At 12 . pairMac) tripBuilder
  <> FB.contramapBuilder pairSep FB.word8
  <> FB.contramapBuilder (word12At 0 . pairMac) tripBuilder
{-# INLINE fixedBuilderTriples #-}

fixedBuilderQuadruples :: FB.Builder Word8 -> FB.Builder Pair
fixedBuilderQuadruples pairBuilder =
     FB.contramapBuilder (word8At 40 . pairMac) pairBuilder
  <> FB.contramapBuilder (word8At 32 . pairMac) pairBuilder
  <> FB.contramapBuilder pairSep FB.word8
  <> FB.contramapBuilder (word8At 24 . pairMac) pairBuilder
  <> FB.contramapBuilder (word8At 16 . pairMac) pairBuilder
  <> FB.contramapBuilder pairSep FB.word8
  <> FB.contramapBuilder (word8At 8 . pairMac) pairBuilder
  <> FB.contramapBuilder (word8At 0 . pairMac) pairBuilder
{-# INLINE fixedBuilderQuadruples #-}

fixedBuilderPairs :: FB.Builder Word8 -> FB.Builder Pair
fixedBuilderPairs pairBuilder =
     FB.contramapBuilder (word8At 40 . pairMac) pairBuilder
  <> FB.contramapBuilder pairSep FB.word8
  <> FB.contramapBuilder (word8At 32 . pairMac) pairBuilder
  <> FB.contramapBuilder pairSep FB.word8
  <> FB.contramapBuilder (word8At 24 . pairMac) pairBuilder
  <> FB.contramapBuilder pairSep FB.word8
  <> FB.contramapBuilder (word8At 16 . pairMac) pairBuilder
  <> FB.contramapBuilder pairSep FB.word8
  <> FB.contramapBuilder (word8At 8 . pairMac) pairBuilder
  <> FB.contramapBuilder pairSep FB.word8
  <> FB.contramapBuilder (word8At 0 . pairMac) pairBuilder
{-# INLINE fixedBuilderPairs #-}

fixedBuilderNoSeparator :: FB.Builder Word8 -> FB.Builder Mac
fixedBuilderNoSeparator hexBuilder =
     FB.contramapBuilder (word8At 40) hexBuilder
  <> FB.contramapBuilder (word8At 32) hexBuilder
  <> FB.contramapBuilder (word8At 24) hexBuilder
  <> FB.contramapBuilder (word8At 16) hexBuilder
  <> FB.contramapBuilder (word8At 8) hexBuilder
  <> FB.contramapBuilder (word8At 0) hexBuilder
{-# INLINE fixedBuilderNoSeparator #-}

word8At :: Int -> Mac -> Word8
word8At i (Mac w) = fromIntegral (unsafeShiftR w i)
{-# INLINE word8At #-}

word12At :: Int -> Mac -> Word12
word12At i (Mac w) = fromIntegral (unsafeShiftR w i)
{-# INLINE word12At #-}

