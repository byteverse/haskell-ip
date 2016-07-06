module Net.IPv4.Text
  ( encode
  , decode
  , decodeEither
  , builder
  , parser
  , print
  ) where

import Prelude hiding (print)
import Net.Types (IPv4(..))
import Net.IPv4
import Data.Text (Text)
import Data.Coerce (coerce)
import qualified Data.Text.IO           as Text
import qualified Data.Attoparsec.Text   as AT
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Net.Internal           as Internal

encode :: IPv4 -> Text
encode = Internal.toDotDecimalText . getIPv4

decodeEither :: Text -> Either String IPv4
decodeEither = coerce . Internal.fromDotDecimalText'

decode :: Text -> Maybe IPv4
decode = Internal.rightToMaybe . decodeEither

builder :: IPv4 -> TBuilder.Builder
builder = Internal.toDotDecimalBuilder . getIPv4

parser :: AT.Parser IPv4
parser = coerce Internal.dotDecimalParser

-- | This exists mostly for testing purposes.
print :: IPv4 -> IO ()
print = Text.putStrLn . encode

