module IPv4Text2 where

import Net.Types (IPv4(..))
import Data.Text (Text)
import qualified Net.IPv4 as IPv4
import qualified Data.Text as Text
import Text.Read (readMaybe)
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Internal (Text(..))
import Data.Word
import Data.ByteString (ByteString)
import Control.Monad.ST
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8  as BC8
import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Text.Lazy         as LText
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Array        as TArray

-----------------------------------------
-- Text Builder implementation. This ends
-- up performing worse than the naive
-- implementation.
-----------------------------------------
encode :: IPv4 -> Text
encode = LText.toStrict . TBuilder.toLazyText . toDotDecimalBuilder

toDotDecimalBuilder :: IPv4 -> TBuilder.Builder
toDotDecimalBuilder (IPv4 w) =
  decimal (255 .&. shiftR w 24 )
  <> dot
  <> decimal (255 .&. shiftR w 16 )
  <> dot
  <> decimal (255 .&. shiftR w 8 )
  <> dot
  <> decimal (255 .&. w)
  where dot = TBuilder.singleton '.'
{-# INLINE toDotDecimalBuilder #-}

