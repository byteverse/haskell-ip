{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wall #-}

{-| An IP data type representing either an IPv4 address or
    an IPv6 address. The user can think of this
    as though it were a sum type. However, to minimize indirections,
    it is actually implemented as an 'IPv6' address, with 'IPv4'
    addresses being represented as an IPv4-mapped IPv6 addresses:

    > +---------+---------+--------------+
    > | 80 bits | 16 bits | 32 bits      |
    > +---------+---------+--------------+
    > | 00...00 | FFFF    | IPv4 address |
    > +---------+---------+--------------+

    All functions and instance methods that deal with textual conversion
    will encode an 'IP' using either dot-decimal notation (for IPv4) or
    RFC 5952 (for IPv6). They will decode an 'IP' from either format
    as well. The 'Show' instance presents an address in as valid haskell code 
    that resembles the formatted address:
    
    >>> decode "192.168.3.100"
    Just (ipv4 192 168 3 100)
    >>> decode "A3F5:12:F26::1466:8B91"
    Just (ipv6 0xa3f5 0x0012 0x0f26 0x0000 0x0000 0x0000 0x1466 0x8b91)
-}

module Net.IP
  ( -- * Pattern Matching
    case_
    -- * Construction
  , ipv4
  , ipv6
  , fromIPv4
  , fromIPv6
    -- * Textual Conversion
    -- ** Text
  , encode
  , decode
    -- ** Printing
  , print
    -- * Types
  , IP(..)
  ) where

import Prelude hiding (print)
import Control.DeepSeq (NFData)
import Data.Bits
import GHC.Generics (Generic)
import Net.IPv6 (IPv6(..))
import Net.IPv4 (IPv4(..))
import Text.Read (Read(..))
import Text.ParserCombinators.ReadPrec ((+++))
import Data.Aeson (FromJSON(..),ToJSON(..))
import Data.Text (Text)
import Data.Word (Word8,Word16)
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6
import qualified Data.Aeson as Aeson
import qualified Data.Text.IO as TIO

-- | Run a function over an 'IP' depending on its status
--   as an 'IPv4' or 'IPv6'.
--
--   >>> case_ IPv4.encode IPv6.encode (ipv4 192 168 2 47)
--   "192.168.2.47"
--
--   >>> addr = ipv6 0x2001 0x0db8 0x0000 0x0000 0x0000 0x0000 0x0000 0x0001
--   >>> case_ IPv4.encode IPv6.encode addr
--   "2001:db8::1"
case_ :: (IPv4 -> a) -> (IPv6 -> a) -> IP -> a
case_ f g (IP addr@(IPv6 w1 w2)) = if w1 == 0 && (0xFFFFFFFF00000000 .&. w2 == 0x0000FFFF00000000)
  then f (IPv4 (fromIntegral w2))
  else g addr

-- | Construct an 'IP' address from the four octets of
--   an IPv4 address.
ipv4 :: Word8 -> Word8 -> Word8 -> Word8 -> IP
ipv4 a b c d = fromIPv4 (IPv4.fromOctets a b c d)

-- | Construct an 'IP' address from the eight 16-bit
--   chunks of an IPv6 address.
ipv6 :: Word16 -> Word16 -> Word16 -> Word16
     -> Word16 -> Word16 -> Word16 -> Word16
     -> IP
ipv6 a b c d e f g h = fromIPv6 (IPv6.fromWord16s a b c d e f g h)

-- | Turn an 'IPv4' into an 'IP'.
fromIPv4 :: IPv4 -> IP
fromIPv4 (IPv4 w) = IP (IPv6 0 (0x0000FFFF00000000 .|. fromIntegral w))

-- | Turn an 'IPv6' into an 'IP'.
fromIPv6 :: IPv6 -> IP
fromIPv6 = IP

-- | Encode an 'IP' as 'Text'.
encode :: IP -> Text
encode = case_ IPv4.encode IPv6.encode

-- | Decode an 'IP' from 'Text'.
decode :: Text -> Maybe IP
decode t = case IPv4.decode t of
  Nothing -> case IPv6.decode t of
    Nothing -> Nothing
    Just v6 -> Just (fromIPv6 v6)
  Just v4 -> Just (fromIPv4 v4)

-- | Print an 'IP' using the textual encoding.
print :: IP -> IO ()
print = TIO.putStrLn . encode

-- | A 32-bit 'IPv4' address or a 128-bit 'IPv6' address. Internally, this
--   is just represented as an 'IPv6' address. The functions provided
--   in @Net.IP@ help simulate constructing and pattern matching on values
--   of this type. All functions and typeclass methods that convert
--   'IP' values to text will display it as an 'IPv4' address if possible.
newtype IP = IP { getIP :: IPv6 }
  deriving (Eq,Ord,Generic)

instance NFData IP

instance Show IP where
  showsPrec p = case_ (showsPrec p) (showsPrec p)

instance Read IP where
  readPrec = fmap fromIPv4 readPrec +++ fmap fromIPv6 readPrec

instance ToJSON IP where
  toJSON = Aeson.String . encode

instance FromJSON IP where
  parseJSON = Aeson.withText "IP" $ \t -> case decode t of
    Nothing -> fail "Could not parse IP address"
    Just addr -> return addr

