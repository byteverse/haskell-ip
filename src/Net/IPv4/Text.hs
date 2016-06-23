module Net.IPv4.Text
  ( encode
  , decode
  , decodeEither
  , builder
  , parser
  ) where

import Net.IPv4
import Net.Internal (rightToMaybe)
import Data.Text (Text)
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text.Lazy.Builder as TBuilder

encode :: IPv4 -> Text
encode = toDotDecimalText

decodeEither :: Text -> Either String IPv4
decodeEither = fromDotDecimalText'

decode :: Text -> Maybe IPv4
decode = rightToMaybe . decodeEither

builder :: IPv4 -> TBuilder.Builder
builder = toDotDecimalBuilder

parser :: AT.Parser IPv4
parser = dotDecimalParser

