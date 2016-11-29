module Net.Mac.ByteString.Char8
  ( encode
  , decode
  , builder
  , parser
  , parserWith
  ) where

import Net.Types (Mac(..),MacEncoding(..),MacDecoding(..))
import Net.Mac (fromOctets)
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString.Lazy.Builder (Builder)
import Net.Internal (rightToMaybe,c2w)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Data.Word (Word8)
import Data.Bits (unsafeShiftL)
import Control.Monad
import qualified Data.ByteString.Builder as Builder
import qualified Data.Attoparsec.ByteString as ABW
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Net.Mac.Text as MacText

-- | This is a mediocre implementation that should
--   be rewritten.
encode :: Mac -> ByteString
encode = encodeUtf8 . MacText.encode

-- | This is a mediocre implementation that should
--   be rewritten.
decode :: ByteString -> Maybe Mac
decode = MacText.decode <=< rightToMaybe . decodeUtf8'

-- | Make a bytestring builder from a 'Mac' address
--   using a colon as the separator.
builder :: Mac -> Builder
builder = Builder.byteString . encode

-- | Parser for a 'Mac' address using with a colon as the
--   separator (i.e. @FA:43:B2:C0:0F:99@).
parser :: Parser Mac
parser = parserWith defDecoding

-- | Parser for a 'Mac' address using to the provided
--   settings.
parserWith :: MacDecoding -> Parser Mac
parserWith x = case x of
  MacDecodingPairs s -> parserPairs (c2w s)
  MacDecodingTriples s -> parserTriples (c2w s)
  MacDecodingQuadruples s -> parserQuadruples (c2w s)
  MacDecodingNoSeparator -> parserNoSeparator
  MacDecodingLenient -> parserLenient

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

defDecoding :: MacDecoding
defDecoding = MacDecodingPairs ':'

