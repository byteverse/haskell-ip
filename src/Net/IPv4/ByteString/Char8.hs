module Net.IPv4.ByteString.Char8
  ( encode
  , decode
  , builder
  , parser
  ) where

import Net.IPv4
import Control.Monad
import qualified Net.IPv4.Text as IPv4Text
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.ByteString.Builder as Builder
import Net.Internal (rightToMaybe)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString.Lazy.Builder (Builder)

-- | This should be rewritten to not create 'Text' as an 
--   intermediate step.
encode :: IPv4 -> ByteString
encode = encodeUtf8 . IPv4Text.encode

-- | This should also be rewritten
decode :: ByteString -> Maybe IPv4
decode = IPv4Text.decode <=< rightToMaybe . decodeUtf8'

builder :: IPv4 -> Builder
builder = Builder.byteString . encode

parser :: Parser IPv4
parser = fromOctets'
  <$> (AB.decimal >>= limitSize)
  <*  AB.char '.'
  <*> (AB.decimal >>= limitSize)
  <*  AB.char '.'
  <*> (AB.decimal >>= limitSize)
  <*  AB.char '.'
  <*> (AB.decimal >>= limitSize)
  where
  limitSize i = 
    if i > 255 
      then fail "All octets in an ip address must be between 0 and 255"
      else return i

