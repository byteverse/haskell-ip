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

import Net.Types (Mac(..),MacEncoding(..),MacDecoding(..))
import Net.Mac (fromOctetsNoCast)
import Data.Text (Text)
import Data.Word (Word8)
import Data.Char (chr)
import qualified Net.Internal as Internal
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text.Lazy.Builder as TBuilder

encode :: Mac -> Text
encode (Mac a b) = Internal.macToTextDefault a b

decodeEitherWith :: MacDecoding -> Text -> Either String Mac
decodeEitherWith = error "decodeEitherWith: write me" -- (MacDecoding separator) =
  -- Internal.macFromText' (fmap w8ToChar separator) fromOctetsNoCast

decodeEither :: Text -> Either String Mac
decodeEither = decodeEitherWith defDecoding

decode :: Text -> Maybe Mac
decode = decodeWith defDecoding

decodeWith :: MacDecoding -> Text -> Maybe Mac
decodeWith d = Internal.rightToMaybe . decodeEitherWith d

-- decodeWith ::

builder :: Mac -> TBuilder.Builder
builder (Mac a b) = TBuilder.fromText (Internal.macToTextDefault a b)

parser :: AT.Parser Mac
parser = parserWith defDecoding

parserWith :: MacDecoding -> AT.Parser Mac
parserWith = error "parserWith: write me" -- (MacDecoding separator) =
  -- Internal.macTextParser (fmap w8ToChar separator) fromOctetsNoCast

encodeWith :: MacEncoding -> Mac -> Text
encodeWith (MacEncoding separator isUpperCase) (Mac a b) =
  Internal.macToTextPreAllocated separator isUpperCase a b

defDecoding :: MacDecoding
defDecoding = MacDecodingPairs ':'

w8ToChar :: Word8 -> Char
w8ToChar = chr . fromIntegral

