module Naive where

import Net.IPv4 (IPv4(..))
import Data.Text (Text)
import qualified Net.IPv4 as IPv4
import qualified Data.Text as Text
import Text.Read (readMaybe)
import Net.IPv4 (IPv4(..))
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

encodeByteString :: IPv4 -> ByteString
encodeByteString = encodeUtf8 . encodeText

encodeText :: IPv4 -> Text
encodeText i = Text.pack $ concat
  [ show a
  , "."
  , show b
  , "."
  , show c
  , "."
  , show d
  ]
  where (a,b,c,d) = IPv4.toOctets i

decodeText :: Text -> Maybe IPv4
decodeText t =
  case mapM (readMaybe . Text.unpack) (Text.splitOn (Text.pack ".") t) of
    Just [a,b,c,d] -> Just (IPv4.fromOctets a b c d)
    _ -> Nothing

