module Net.IPv4.Range.Text
  ( encode
  , decode
  , decodeEither
  , builder
  , parser
  , print
  ) where

import Prelude hiding (print)
import Net.Types (IPv4Range(..),IPv4(..))
import Data.Text (Text)
import Data.Word (Word8, Word32)
import qualified Data.Text.IO           as Text
import qualified Data.Attoparsec.Text   as AT
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Net.Internal           as Internal

encode :: IPv4Range -> Text
encode (IPv4Range (IPv4 w) r) = Internal.rangeToDotDecimalText w r

decodeEither :: Text -> Either String IPv4Range
decodeEither = Internal.rangeFromDotDecimalText' mkIPv4Range

decode :: Text -> Maybe IPv4Range
decode = Internal.rightToMaybe . decodeEither

builder :: IPv4Range -> TBuilder.Builder
builder (IPv4Range (IPv4 w) r) = Internal.rangeToDotDecimalBuilder w r

parser :: AT.Parser IPv4Range
parser = Internal.dotDecimalRangeParser mkIPv4Range

-- | This exists mostly for testing purposes.
print :: IPv4Range -> IO ()
print = Text.putStrLn . encode

-- internal function
mkIPv4Range :: Word32 -> Word8 -> IPv4Range
mkIPv4Range w = IPv4Range (IPv4 w)
{-# INLINE mkIPv4Range #-}

