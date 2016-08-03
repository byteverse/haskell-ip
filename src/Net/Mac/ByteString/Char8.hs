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
import Net.Internal (rightToMaybe)
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
parserWith (MacDecoding separator) = fromOctets
  <$> parseTwoHex
  <*  parseSeparator
  <*> parseTwoHex
  <*  parseSeparator
  <*> parseTwoHex
  <*  parseSeparator
  <*> parseTwoHex
  <*  parseSeparator
  <*> parseTwoHex
  <*  parseSeparator
  <*> parseTwoHex
  where
  parseSeparator = case separator of
    Just c -> ABW.word8 c
    Nothing -> return 60 -- character is unused

parseTwoHex :: Parser Word8
parseTwoHex = do
  a <- ABW.anyWord8 >>= parseWord8Hex
  b <- ABW.anyWord8 >>= parseWord8Hex
  return (unsafeShiftL a 4 + b)

parseWord8Hex :: Word8 -> Parser Word8
parseWord8Hex w
  | w >= 48 && w <= 57  = return (w - 48)
  | w >= 65 && w <= 70  = return (w - 55)
  | w >= 97 && w <= 102 = return (w - 87)
  | otherwise = fail "invalid hexadecimal character"

defDecoding :: MacDecoding
defDecoding = MacDecoding (Just 58)

