module Net.IPv4.ByteString.Char8
  ( encode
  , decode
  , builder
  , parser
  ) where

import Net.IPv4
import Data.ByteString (ByteString)
import Data.Attoparsec.Text (Parser)
import Data.ByteString.Lazy.Builder (Builder)

encode :: IPv4 -> ByteString
encode = error "write me"

decode :: ByteString -> Either String IPv4
decode = error "write me"

builder :: IPv4 -> Builder
builder = error "write me"

parser :: Parser IPv4
parser = error "write me"

