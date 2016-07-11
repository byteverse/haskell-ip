{-| An IPv4 data type

    This module provides the IPv4 data type and functions for working
    with it. There are also encoding and decoding functions provided
    in this module, but they should be imported from
    @Net.IPv4.Text@ and @Net.IPv4.ByteString.Char8@ instead. They are
    defined here so that the 'FromJSON' and 'ToJSON' instances can
    use them.

    At some point, a highly efficient IPv4-to-ByteString function needs
    to be added to this module to take advantage of @aeson@'s new
    @toEncoding@ method.
-}

module Net.IPv4
  ( -- * Conversion Functions
    fromOctets
  , toOctets
    -- * Special IP Addresses
  , any
  , loopback
  , broadcast
    -- * Range Predicates
  , private
  , reserved
  , public
  ) where

import Prelude hiding (any)
import Net.Types (IPv4(..))
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)
import Data.Word
import Data.Int
import Data.Hashable
import Data.Aeson (FromJSON(..),ToJSON(..))
import GHC.Generics (Generic)
import Net.Internal (attoparsecParseJSON,rightToMaybe)
import Control.Monad
import Data.Text.Internal (Text(..))
import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)
import Data.ByteString (ByteString)
import Data.Vector.Generic.Mutable      (MVector(..))
import Control.Monad.Primitive          (PrimMonad,PrimState)
import qualified Net.Internal           as Internal
import qualified Data.Text.Lazy         as LText
import qualified Data.Text.IO           as Text
import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Unsafe as ByteString

-- $setup
--
-- These are here to get doctest's property checking to work
--
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> instance Arbitrary IPv4 where { arbitrary = fmap IPv4 arbitrary }
--

-- | Create an 'IPv4' address from four octets. The first argument
--   is the most significant octet. The last argument is the least
--   significant.
--
--   Since the 'Show' and 'Read' instances for 'IPv4' are not generally
--   usefully, this function is the recommened way to create 'IPv4' addresses.
--   For example:
--
--   >>> fromOctets 192 168 1 1
--   IPv4 {getIPv4 = 3232235777}
--
fromOctets :: Word8 -> Word8 -> Word8 -> Word8 -> IPv4
fromOctets a b c d = IPv4 $ Internal.fromOctets'
  (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)

-- | Convert an 'IPv4' address into a quadruple of octets. The first
--   element in the quadruple is the most significant octet. The last
--   element is the least significant octet.
toOctets :: IPv4 -> (Word8,Word8,Word8,Word8)
toOctets (IPv4 w) =
  ( fromIntegral (shiftR w 24)
  , fromIntegral (shiftR w 16)
  , fromIntegral (shiftR w 8)
  , fromIntegral w
  )

-- | The IP address representing any host: @0.0.0.0@
any :: IPv4
any = IPv4 0

-- | The loopback IP address: @127.0.0.1@
loopback :: IPv4
loopback = fromOctets 127 0 0 1

-- | The broadcast IP address: @127.0.0.1@
broadcast :: IPv4
broadcast = fromOctets 255 255 255 255

-- | Checks to see if the 'IPv4' address belongs to a private
-- network. The three private networks that are checked are
-- @10.0.0.0/8@, @172.16.0.0/12@, and @192.168.0.0/16@.
private :: IPv4 -> Bool
private (IPv4 w) =
     Internal.mask8  .&. w == Internal.p24
  || Internal.mask12 .&. w == Internal.p20
  || Internal.mask16 .&. w == Internal.p16

-- | Checks to see if the 'IPv4' address belongs to a reserved
-- network. This includes the three private networks that 'private'
-- checks along with several other ranges that are not used
-- on the public Internet.
reserved :: IPv4 -> Bool
reserved =
  let a = Internal.fromOctets' 0 0 0 0
      b = Internal.fromOctets' 100 64 0 0
      c = Internal.fromOctets' 127 0 0 0
      d = Internal.fromOctets' 169 254 0 0
      e = Internal.fromOctets' 192 0 0 0
      f = Internal.fromOctets' 192 0 2 0
      g = Internal.fromOctets' 192 88 99 0
      h = Internal.fromOctets' 198 18 0 0
      i = Internal.fromOctets' 198 51 100 0
      j = Internal.fromOctets' 203 0 113 0
      k = Internal.fromOctets' 224 0 0 0
      l = Internal.fromOctets' 240 0 0 0
      m = Internal.fromOctets' 255 255 255 255
  in \(IPv4 w) -> Internal.mask8  .&. w == Internal.p24
               || Internal.mask12 .&. w == Internal.p20
               || Internal.mask16 .&. w == Internal.p16
               || Internal.mask8  .&. w == a
               || Internal.mask10 .&. w == b
               || Internal.mask16 .&. w == d
               || Internal.mask24 .&. w == e
               || Internal.mask24 .&. w == g
               || Internal.mask15 .&. w == h
               || Internal.mask24 .&. w == i
               || Internal.mask24 .&. w == j
               || Internal.mask4  .&. w == k
               || Internal.mask4  .&. w == l
               || Internal.mask32 .&. w == m

-- | Checks to see if the 'IPv4' address is publicly routable.
--
-- prop> public x == not (reserved x)
public :: IPv4 -> Bool
public = not . reserved

-- | $internal
-- Everything below here is not part of the stable API. Many of these
-- functions must live here because they are needed for the 'ToJSON' and
-- 'FromJSON' instances. Hopefully, at some point, these can be removed
-- from this module.

-- This only exists for doctests. Do not use it.
-- prAddr :: IPv4 -> IO ()
-- prAddr (IPv4 addr) = Text.putStrLn (Internal.toDotDecimalText addr)


