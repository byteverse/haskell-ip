module Net.Mac.Text
  ( encode
  , decode
  , decodeEither
  , builder
  , parser
  ) where

import Net.Mac
import Data.Text (Text)
import Net.Internal (rightToMaybe)
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text.Lazy.Builder as TBuilder

encode :: Mac -> Text
encode = toText

decodeEither :: Text -> Either String Mac
decodeEither = fromText'

decode :: Text -> Maybe Mac
decode = rightToMaybe . decodeEither

builder :: Mac -> TBuilder.Builder
builder = toTextBuilder

parser :: AT.Parser Mac
parser = textParser

