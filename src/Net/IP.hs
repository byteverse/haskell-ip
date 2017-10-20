module Net.IP
  ( -- * Pattern Matching
    case_
  , ipv4
  , ipv6
    -- * Types
  , IP(..)
  ) where

import Data.Bits
import Net.IPv6 (IPv6(..))
import Net.IPv4 (IPv4(..))

case_ :: (IPv4 -> a) -> (IPv6 -> a) -> IP -> a
case_ f g (IP addr@(IPv6 w1 w2)) = if w1 == 0 && (0xFFFFFFFF00000000 .&. w2 == 0x0000FFFF00000000)
  then f (IPv4 (fromIntegral w2))
  else g addr

-- | If the address is an 'IPv4' address, return the address.
ipv4 :: IP -> Maybe IPv4
ipv4 = case_ Just (const Nothing)

-- | If the address is an 'IPv6' address, and if it is not
--   an IPv4-mapped IPv6 address, return the address.
ipv6 :: IP -> Maybe IPv6
ipv6 = case_ (const Nothing) Just

-- | A 32-bit 'IPv4' address or a 128-bit 'IPv6' address. Internally, this
--   is just represented as an 'IPv6' address. The functions provided
--   in @Net.IP@ help simulate pattern matching on it.
newtype IP = IP { getIP :: IPv6 }
  deriving (Eq,Ord,Show,Read)

