-- | This module exists for the convenience of those who need a
-- 'String' representation of an 'IPv4' address. Using this module
-- is discouraged unless the end user is working with a library
-- that can only use 'String' to deal with textual data (such as
-- @pandoc@, @hxr@, or @network@).
--
module Net.IPv4.String
  ( encode
  , decode
  , decodeEither
  ) where

import Net.Types (IPv4(..))
import qualified Data.Text as Text
import qualified Net.IPv4.Text as N

encode :: IPv4 -> String
encode = Text.unpack . N.encode

decode :: String -> Maybe IPv4
decode = N.decode . Text.pack

decodeEither :: String -> Either String IPv4
decodeEither = N.decodeEither . Text.pack

