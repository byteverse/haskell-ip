module IPv4Text2 where

import Data.Bits (shiftR, (.&.))
import Data.Text (Text)
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as TBuilder
import Data.Text.Lazy.Builder.Int (decimal)
import Net.Types (IPv4 (..))

-----------------------------------------
-- Text Builder implementation. This ends
-- up performing worse than the naive
-- implementation.
-----------------------------------------
encode :: IPv4 -> Text
encode = LText.toStrict . TBuilder.toLazyText . toDotDecimalBuilder

toDotDecimalBuilder :: IPv4 -> TBuilder.Builder
toDotDecimalBuilder (IPv4 w) =
  decimal (255 .&. shiftR w 24)
    <> dot
    <> decimal (255 .&. shiftR w 16)
    <> dot
    <> decimal (255 .&. shiftR w 8)
    <> dot
    <> decimal (255 .&. w)
 where
  dot = TBuilder.singleton '.'
{-# INLINE toDotDecimalBuilder #-}
