module Net.Mac.Text
  ( encode
  , encodeWith
  , decode
  , decodeWith
  , decodeEither
  , decodeEitherWith
  , builder
  , parser
  , parserWith
  ) where

import Net.Types (Mac(..),MacCodec(..),MacGrouping(..))
import Net.Mac (fromOctetsNoCast)
import Data.Text (Text)
import Data.Word (Word8)
import Data.Char (chr)
import qualified Net.Internal as Internal
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text.Lazy.Builder as TBuilder

encode :: Mac -> Text
encode (Mac a b) = Internal.macToTextDefault a b

decodeEitherWith :: MacCodec -> Text -> Either String Mac
decodeEitherWith = error "decodeEitherWith: write me" -- (MacDecoding separator) =
  -- Internal.macFromText' (fmap w8ToChar separator) fromOctetsNoCast

decodeEither :: Text -> Either String Mac
decodeEither = decodeEitherWith defCodec

decode :: Text -> Maybe Mac
decode = decodeWith defCodec

decodeWith :: MacCodec -> Text -> Maybe Mac
decodeWith d = Internal.rightToMaybe . decodeEitherWith d

-- decodeWith ::

builder :: Mac -> TBuilder.Builder
builder (Mac a b) = TBuilder.fromText (Internal.macToTextDefault a b)

parser :: AT.Parser Mac
parser = parserWith defCodec

parserWith :: MacCodec -> AT.Parser Mac
parserWith = error "parserWith: write me" -- (MacDecoding separator) =
  -- Internal.macTextParser (fmap w8ToChar separator) fromOctetsNoCast

encodeWith :: MacCodec -> Mac -> Text
encodeWith (MacCodec grouping isUpperCase) (Mac a b) =
  Internal.macToTextPreAllocated grouping isUpperCase a b

defCodec :: MacCodec
defCodec = MacCodec (MacGroupingPairs ':') False

w8ToChar :: Word8 -> Char
w8ToChar = chr . fromIntegral

