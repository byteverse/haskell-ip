module Net.Mac.Text
  ( encode
  , decode
  , decodeEither
  , builder
  , parser
  ) where

import Net.Types (Mac(..))
import Net.Mac (fromOctetsNoCast)
import Data.Text (Text)
import qualified Net.Internal as Internal
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text.Lazy.Builder as TBuilder

encode :: Mac -> Text
encode (Mac a b) = Internal.macToText a b

decodeEither :: Text -> Either String Mac
decodeEither = Internal.macFromText' fromOctetsNoCast

decode :: Text -> Maybe Mac
decode = Internal.rightToMaybe . decodeEither

builder :: Mac -> TBuilder.Builder
builder (Mac a b) = Internal.macToTextBuilder a b

parser :: AT.Parser Mac
parser = Internal.macTextParser fromOctetsNoCast

