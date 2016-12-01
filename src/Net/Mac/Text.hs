module Net.Mac.Text
  ( encode
  , encodeWith
  , decode
  , decodeWith
  , decodeEither
  , decodeEitherWith
  , builder
  , parser
  , parserWith
  ) where

import Net.Types (Mac(..),MacCodec(..),MacGrouping(..))
import Net.Mac (fromOctets)
-- import Net.Mac (fromOctetsNoCast)
import Data.Text (Text)
import Data.Word (Word8)
import Data.Word.Synthetic (Word12)
import Data.Char (chr)
import Data.Attoparsec.Text (Parser)
import Net.Internal (rightToMaybe,c2w)
import Data.Bits (unsafeShiftL,unsafeShiftR)
import Data.Monoid
import qualified Net.Internal as Internal
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Builder.Fixed as FB

encode :: Mac -> Text
encode = encodeWith defCodec -- Internal.macToTextDefault w

encodeWith :: MacCodec -> Mac -> Text
encodeWith (MacCodec g u) m = case g of
  MacGroupingNoSeparator -> case u of
    True -> FB.run (fixedBuilderNoSeparator FB.word8HexFixedUpper) m
    False -> FB.run (fixedBuilderNoSeparator FB.word8HexFixedLower) m
  MacGroupingPairs c -> case u of
    True -> FB.run (fixedBuilderPairs FB.word8HexFixedUpper) (Pair c m)
    False -> FB.run (fixedBuilderPairs FB.word8HexFixedLower) (Pair c m)
    -- withCasedBuilder u $ \bw8 -> FB.run (fixedBuilderPairs bw8) (Pair c m)
  MacGroupingTriples c -> case u of
    True -> FB.run (fixedBuilderTriples FB.word12HexFixedUpper) (Pair c m)
    False -> FB.run (fixedBuilderTriples FB.word12HexFixedLower) (Pair c m)
  MacGroupingQuadruples c -> case u of
    True -> FB.run (fixedBuilderQuadruples FB.word8HexFixedUpper) (Pair c m)
    False -> FB.run (fixedBuilderQuadruples FB.word8HexFixedLower) (Pair c m)

withCasedBuilder :: Bool -> (FB.Builder Word8 -> a) -> a
withCasedBuilder x f = case x of
  True -> f FB.word8HexFixedUpper
  False -> f FB.word8HexFixedLower
{-# INLINE withCasedBuilder #-}


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

w8ToChar :: Word8 -> Char
w8ToChar = chr . fromIntegral

parserQuadruples :: Char -> Parser Mac
parserQuadruples s = fromOctets
  <$> parseTwoHex <*> parseTwoHex <* AT.char s
  <*> parseTwoHex <*> parseTwoHex <* AT.char s
  <*> parseTwoHex <*> parseTwoHex

parserPairs :: Char -> Parser Mac
parserPairs s = fromOctets
  <$> parseTwoHex <* AT.char s
  <*> parseTwoHex <* AT.char s
  <*> parseTwoHex <* AT.char s
  <*> parseTwoHex <* AT.char s
  <*> parseTwoHex <* AT.char s
  <*> parseTwoHex

parserTriples :: Char -> Parser Mac
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

parserNoSeparator :: Parser Mac
parserNoSeparator = fromOctets
  <$> parseTwoHex
  <*> parseTwoHex
  <*> parseTwoHex
  <*> parseTwoHex
  <*> parseTwoHex
  <*> parseTwoHex

parseTwoHex :: Parser Word8
parseTwoHex = do
  a <- AT.anyChar >>= parseCharHex
  b <- AT.anyChar >>= parseCharHex
  return (unsafeShiftL a 4 + b)

tryParseCharHex :: Parser Word8 -> Char -> Parser Word8
tryParseCharHex a c
  | w >= 48 && w <= 57 = return (w - 48)
  | w >= 65 && w <= 70 = return (w - 55)
  | w >= 97 && w <= 102 = return (w - 87)
  | otherwise = a
  where w = c2w c

parseOneHex :: Parser Word8
parseOneHex = AT.anyChar >>= parseCharHex

parseCharHex :: Char -> Parser Word8
parseCharHex = tryParseCharHex (fail "invalid hexadecimal character")

withCasedBuilderTriple :: Bool -> (FB.Builder Word12 -> a) -> a
withCasedBuilderTriple x f = case x of
  True -> f FB.word12HexFixedUpper
  False -> f FB.word12HexFixedLower
{-# INLINE withCasedBuilderTriple #-}

data Pair = Pair
  { pairSep :: {-# UNPACK #-} !Char
  , pairMac :: {-# UNPACK #-} !Mac
  }

fixedBuilderTriples :: FB.Builder Word12 -> FB.Builder Pair
fixedBuilderTriples tripBuilder =
     FB.contramapBuilder (word12At 36 . pairMac) tripBuilder
  <> FB.contramapBuilder pairSep FB.char
  <> FB.contramapBuilder (word12At 24 . pairMac) tripBuilder
  <> FB.contramapBuilder pairSep FB.char
  <> FB.contramapBuilder (word12At 12 . pairMac) tripBuilder
  <> FB.contramapBuilder pairSep FB.char
  <> FB.contramapBuilder (word12At 0 . pairMac) tripBuilder
{-# INLINE fixedBuilderTriples #-}

fixedBuilderNoSeparator :: FB.Builder Word8 -> FB.Builder Mac
fixedBuilderNoSeparator hexBuilder =
     FB.contramapBuilder (word8At 40) hexBuilder
  <> FB.contramapBuilder (word8At 32) hexBuilder
  <> FB.contramapBuilder (word8At 24) hexBuilder
  <> FB.contramapBuilder (word8At 16) hexBuilder
  <> FB.contramapBuilder (word8At 8) hexBuilder
  <> FB.contramapBuilder (word8At 0) hexBuilder
{-# INLINE fixedBuilderNoSeparator #-}

fixedBuilderQuadruples :: FB.Builder Word8 -> FB.Builder Pair
fixedBuilderQuadruples pairBuilder =
     FB.contramapBuilder (word8At 40 . pairMac) pairBuilder
  <> FB.contramapBuilder (word8At 32 . pairMac) pairBuilder
  <> FB.contramapBuilder pairSep FB.char
  <> FB.contramapBuilder (word8At 24 . pairMac) pairBuilder
  <> FB.contramapBuilder (word8At 16 . pairMac) pairBuilder
  <> FB.contramapBuilder pairSep FB.char
  <> FB.contramapBuilder (word8At 8 . pairMac) pairBuilder
  <> FB.contramapBuilder (word8At 0 . pairMac) pairBuilder
{-# INLINE fixedBuilderQuadruples #-}

fixedBuilderPairs :: FB.Builder Word8 -> FB.Builder Pair
fixedBuilderPairs pairBuilder =
     FB.contramapBuilder (word8At 40 . pairMac) pairBuilder
  <> FB.contramapBuilder pairSep FB.char
  <> FB.contramapBuilder (word8At 32 . pairMac) pairBuilder
  <> FB.contramapBuilder pairSep FB.char
  <> FB.contramapBuilder (word8At 24 . pairMac) pairBuilder
  <> FB.contramapBuilder pairSep FB.char
  <> FB.contramapBuilder (word8At 16 . pairMac) pairBuilder
  <> FB.contramapBuilder pairSep FB.char
  <> FB.contramapBuilder (word8At 8 . pairMac) pairBuilder
  <> FB.contramapBuilder pairSep FB.char
  <> FB.contramapBuilder (word8At 0 . pairMac) pairBuilder
{-# INLINE fixedBuilderPairs #-}

word8At :: Int -> Mac -> Word8
word8At i (Mac w) = fromIntegral (unsafeShiftR w i)
{-# INLINE word8At #-}

word12At :: Int -> Mac -> Word12
word12At i (Mac w) = fromIntegral (unsafeShiftR w i)
{-# INLINE word12At #-}
