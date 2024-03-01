module Naive where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Net.IPv4 as IPv4
import Net.Types (IPv4 (..))
import Text.Read (readMaybe)

encodeByteString :: IPv4 -> ByteString
encodeByteString = encodeUtf8 . encodeText

encodeText :: IPv4 -> Text
encodeText i =
  Text.pack $
    concat
      [ show a
      , "."
      , show b
      , "."
      , show c
      , "."
      , show d
      ]
 where
  (a, b, c, d) = IPv4.toOctets i

decodeText :: Text -> Maybe IPv4
decodeText t =
  case mapM (readMaybe . Text.unpack) (Text.splitOn (Text.pack ".") t) of
    Just [a, b, c, d] -> Just (IPv4.fromOctets a b c d)
    _ -> Nothing
