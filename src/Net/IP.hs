{-# LANGUAGE DataKinds #-}
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
  , isIPv4
  , isIPv6
    -- * Construction
  , ipv4
  , ipv6
  , fromIPv4
  , fromIPv6
    -- * Textual Conversion
    -- ** Text
  , encode
  , encodeShort
  , decode
  , boundedBuilderUtf8
    -- ** Printing
  , print
    -- * Types
  , IP(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON(..),ToJSON(..))
import Data.Bits
import Data.Text (Text)
import Data.WideWord (Word128(..))
import Data.Word (Word8,Word16)
import GHC.Generics (Generic)
import Net.IPv4 (IPv4(..))
import Net.IPv6 (IPv6(..))
import Prelude hiding (print)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.Read (Read(..))
import Data.Text.Short (ShortText)

import qualified Arithmetic.Lte as Lte
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray.Builder.Bounded as BB
import qualified Data.Text.IO as TIO
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6

-- $setup
-- >>> :set -XOverloadedStrings

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
-- Note: rather than performing the masking operations on the 'Word128',
-- we unwrap the 'Word64's, as that's probably a bit more efficient, and
-- we might need the lower word anyway.
case_ f g (IP addr@(IPv6 (Word128 w1 w2))) = if w1 == 0 && (0xFFFFFFFF00000000 .&. w2 == 0x0000FFFF00000000)
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
fromIPv4 (IPv4 w) = IP (IPv6 (Word128 0 (0x0000FFFF00000000 .|. fromIntegral w)))

-- | Turn an 'IPv6' into an 'IP'.
fromIPv6 :: IPv6 -> IP
fromIPv6 = IP

-- | Encode an 'IP' as 'Text'.
--
--   >>> encode (ipv4 10 0 0 25)
--   "10.0.0.25"
--
--   >>> encode (ipv6 0x3124 0x0 0x0 0xDEAD 0xCAFE 0xFF 0xFE00 0x1)
--   "3124::dead:cafe:ff:fe00:1"
encode :: IP -> Text
encode = case_ IPv4.encode IPv6.encode

-- | Encode an 'IP' as 'ShortText'.
--
--   >>> encodeShort (ipv4 10 0 1 26)
--   "10.0.1.26"
--
--   >>> encodeShort (ipv6 0x3124 0x0 0x0 0xDEAD 0xCAFE 0xFF 0xFE01 0x0000)
--   "3124::dead:cafe:ff:fe01:0"
encodeShort :: IP -> ShortText
encodeShort = case_ IPv4.encodeShort IPv6.encodeShort

-- | Encode an 'IP' as a bounded bytearray builder.
boundedBuilderUtf8 :: IP -> BB.Builder 39
boundedBuilderUtf8 = case_
  (\y -> BB.weaken Lte.constant (IPv4.boundedBuilderUtf8 y))
  IPv6.boundedBuilderUtf8

-- | Decode an 'IP' from 'Text'.
--
--   >>> decode "10.0.0.25"
--   Just (ipv4 10 0 0 25)
--
--   >>> fmap isIPv4 (decode "10.0.0.25")
--   Just True
--
--   >>> decode "3124::dead:cafe:ff:fe00:1"
--   Just (ipv6 0x3124 0x0000 0x0000 0xdead 0xcafe 0x00ff 0xfe00 0x0001)
--
--   >>> fmap isIPv6 (decode "3124::dead:cafe:ff:fe00:1")
--   Just True
decode :: Text -> Maybe IP
decode t = case IPv4.decode t of
  Nothing -> case IPv6.decode t of
    Nothing -> Nothing
    Just v6 -> Just (fromIPv6 v6)
  Just v4 -> Just (fromIPv4 v4)

-- | Is the 'IP' an IPv4 address?
--
--   >>> isIPv4 (ipv4 10 0 0 25)
--   True
--
--   >>> isIPv4 (ipv6 0x3124 0x0 0x0 0xDEAD 0xCAFE 0xFF 0xFE00 0x1)
--   False
isIPv4 :: IP -> Bool
isIPv4 = case_ (const True) (const False)
{-# inline isIPv4 #-}

-- | Is the 'IP' an IPv6 address?
--
--   >>> isIPv6 (ipv4 10 0 0 25)
--   False
--
--   >>> isIPv6 (ipv6 0x3124 0x0 0x0 0xDEAD 0xCAFE 0xFF 0xFE00 0x1)
--   True
isIPv6 :: IP -> Bool
isIPv6 = case_ (const False) (const True)
{-# inline isIPv6 #-}

-- | Print an 'IP' using the textual encoding. This exists mostly for
--   debugging purposes.
--
--   >>> print (ipv4 10 0 0 25)
--   10.0.0.25
--
--   >>> print (ipv6 0x3124 0x0 0x0 0xDEAD 0xCAFE 0xFF 0xFE00 0x1)
--   3124::dead:cafe:ff:fe00:1
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

