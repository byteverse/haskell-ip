module Net.IP where

import Net.Types
import Data.Bits

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

