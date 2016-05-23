module Net.Mac.ByteString.Char8
  ( encode
  , decode
  , builder
  , parser
  ) where

import Net.Mac
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString.Lazy.Builder (Builder)
import Net.Internal (rightToMaybe)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Control.Monad
import qualified Data.ByteString.Builder as Builder
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Net.Mac.Text as MacText

-- | This is a bad implementation that should be rewritten
encode :: Mac -> ByteString
encode = encodeUtf8 . MacText.encode

-- | This is a bad implementation that should be rewritten
decode :: ByteString -> Maybe Mac
decode = MacText.decode <=< rightToMaybe . decodeUtf8'

builder :: Mac -> Builder
builder = Builder.byteString . encode

parser :: Parser Mac
parser = fromOctets'
  <$> (AB.hexadecimal >>= limitSize)
  <*  AB.char ':'
  <*> (AB.hexadecimal >>= limitSize)
  <*  AB.char ':'
  <*> (AB.hexadecimal >>= limitSize)
  <*  AB.char ':'
  <*> (AB.hexadecimal >>= limitSize)
  <*  AB.char ':'
  <*> (AB.hexadecimal >>= limitSize)
  <*  AB.char ':'
  <*> (AB.hexadecimal >>= limitSize)
  where
  limitSize i = 
    if i > 255 
      then fail "All octets in a mac address must be between 00 and FF"
      else return i

