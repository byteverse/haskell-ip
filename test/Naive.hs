module Naive where

import Net.IPv4 (IPv4(..))
import Data.Text (Text)
import qualified Net.IPv4 as IPv4
import qualified Data.Text as Text
import Text.Read (readMaybe)

ipv4ToTextNaive :: IPv4 -> Text
ipv4ToTextNaive i = Text.pack $ concat
  [ show a
  , "."
  , show b
  , "."
  , show c
  , "."
  , show d
  ]
  where (a,b,c,d) = IPv4.toOctets i

ipv4FromTextNaive :: Text -> Maybe IPv4
ipv4FromTextNaive t = 
  case mapM (readMaybe . Text.unpack) (Text.splitOn (Text.pack ".") t) of
    Just [a,b,c,d] -> Just (IPv4.fromOctets a b c d)
    _ -> Nothing

