module Net.Mac.ByteString.Char8
  ( encode
  , decode
  , builder
  , parser
  ) where

import Net.Mac
import Data.ByteString (ByteString)
import Data.Attoparsec.Text (Parser)
import Data.ByteString.Lazy.Builder (Builder)

encode :: Mac -> ByteString
encode = error "write me"

decode :: ByteString -> Either String Mac
decode = error "write me"

builder :: Mac -> Builder
builder = error "write me"

parser :: Parser Mac
parser = error "write me"

