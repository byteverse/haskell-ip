module Net.Mac.Text
  ( encode
  , decode
  , decodeEither
  , builder
  , parser
  , encodeWith
  ) where

import Net.Types (Mac(..),MacEncoding(..))
import Net.Mac (fromOctetsNoCast)
import Data.Text (Text)
import qualified Net.Internal as Internal
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text.Lazy.Builder as TBuilder

encode :: Mac -> Text
encode (Mac a b) = Internal.macToTextPreAllocated 58 False a b

decodeEither :: Text -> Either String Mac
decodeEither = Internal.macFromText' fromOctetsNoCast

decode :: Text -> Maybe Mac
decode = Internal.rightToMaybe . decodeEither

builder :: Mac -> TBuilder.Builder
builder (Mac a b) = Internal.macToTextBuilder a b

parser :: AT.Parser Mac
parser = Internal.macTextParser fromOctetsNoCast

encodeWith :: MacEncoding -> Mac -> Text
encodeWith (MacEncoding separator isUpperCase) (Mac a b) =
  Internal.macToTextPreAllocated separator isUpperCase a b

