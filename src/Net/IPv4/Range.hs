module Net.IPv4.Range
  ( -- * Range functions
    normalize
  , contains
  , member
  , lowerInclusive
  , upperInclusive
    -- * Conversion to IPv4
  , toList
  , toGenerator
    -- * Private Ranges
  , private24
  , private20
  , private16
    -- * Internal Functions
    -- $internal
  , prRange
  ) where

import Net.Types (IPv4(..),IPv4Range(..))
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)
import Data.Coerce (coerce)
import Control.Monad
import qualified Net.Internal as Internal
import qualified Net.IPv4     as IPv4
import qualified Data.Text.IO as Text

-- $setup
--
-- These are here to get doctest's property checking to work.
--
-- >>> import qualified Net.IPv4.Text as I
-- >>> import Net.IPv4 (fromOctets)
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> instance Arbitrary IPv4 where { arbitrary = fmap IPv4 arbitrary }
-- >>> instance Arbitrary IPv4Range where { arbitrary = IPv4Range <$> arbitrary <*> arbitrary }
--

-- | Checks to see if an 'IPv4' address belongs in the 'IPv4Range'.
--
-- >>> let ip = fromOctets 10 10 1 92
-- >>> contains (IPv4Range (fromOctets 10 0 0 0) 8) ip
-- True
-- >>> contains (IPv4Range (fromOctets 10 11 0 0) 16) ip
-- False
--
-- Typically, element-testing functions are written to take the element
-- as the first argument and the set as the second argument. This is intentionally
-- written the other way for better performance when iterating over a collection.
-- For example, you might test elements in a list for membership like this:
--
-- >>> let r = IPv4Range (fromOctets 10 10 10 6) 31
-- >>> mapM_ (print . contains r) (take 5 $ iterate succ $ fromOctets 10 10 10 5)
-- False
-- True
-- True
-- False
-- False
--
-- The implementation of 'contains' ensures that (with GHC), the bitmask
-- creation and range normalization only occur once in the above example.
-- They are reused as the list is iterated.
contains :: IPv4Range -> IPv4 -> Bool
contains (IPv4Range (IPv4 wsubnet) len) =
  let theMask = Internal.mask len
      wsubnetNormalized = wsubnet .&. theMask
   in \(IPv4 w) -> (w .&. theMask) == wsubnetNormalized

-- | This is provided to mirror the interface provided by @Data.Set@. It
-- behaves just like 'contains' but with flipped arguments.
--
-- prop> member ip r == contains r ip
member :: IPv4 -> IPv4Range -> Bool
member = flip contains

-- | The inclusive lower bound of an 'IPv4Range'. This is conventionally
--   understood to be the broadcast address of a subnet. For example:
--
-- >>> I.print $ lowerInclusive $ IPv4Range (fromOctets 10 10 1 160) 25
-- 10.10.1.128
--
-- Note that the lower bound of a normalized 'IPv4Range' is simply the
-- ip address of the range:
--
-- prop> lowerInclusive r == ipv4RangeBase (normalize r)
lowerInclusive :: IPv4Range -> IPv4
lowerInclusive (IPv4Range (IPv4 w) len) =
  IPv4 (w .&. Internal.mask len)

upperInclusive :: IPv4Range -> IPv4
upperInclusive (IPv4Range (IPv4 w) len) =
  let theInvertedMask = shiftR 0xffffffff (fromIntegral len)
      theMask = complement theInvertedMask
   in IPv4 ((w .&. theMask) .|. theInvertedMask)

-- | Convert an 'IPv4Range' into a list of the 'IPv4' addresses that
--   are in it.
-- >>> let r = IPv4Range (fromOctets 192 168 1 8) 30
-- >>> mapM_ I.print (toList r)
-- 192.168.1.8
-- 192.168.1.9
-- 192.168.1.10
-- 192.168.1.11

toList :: IPv4Range -> [IPv4]
toList (IPv4Range (IPv4 ip) len) = 
  let totalAddrs = Internal.countAddrs len
   in coerce (Internal.wordSuccessors totalAddrs ip)

toGenerator :: MonadPlus m => IPv4Range -> m IPv4
toGenerator (IPv4Range (IPv4 ip) len) =  
  let totalAddrs = Internal.countAddrs len
   in Internal.wordSuccessorsM IPv4 totalAddrs ip

-- | The RFC1918 24-bit block. Subnet mask: @10.0.0.0/8@
private24 :: IPv4Range
private24 = IPv4Range (IPv4 Internal.p24) 8

-- | The RFC1918 20-bit block. Subnet mask: @172.16.0.0/12@
private20 :: IPv4Range
private20  = IPv4Range (IPv4 Internal.p20) 12

-- | The RFC1918 16-bit block. Subnet mask: @192.168.0.0/16@
private16 :: IPv4Range
private16 = IPv4Range (IPv4 Internal.p16) 16

-- | Normalize an 'IPv4Range'. The first result of this is that the
-- 'IPv4' inside the 'IPv4Range' is changed so that the insignificant
-- bits are zeroed out. For example:
--
-- >>> prRange $ normalize $ IPv4Range (fromOctets 192 168 1 19) 24
-- 192.168.1.0/24
-- >>> prRange $ normalize $ IPv4Range (fromOctets 192 168 1 163) 28
-- 192.168.1.160/28
--
-- The second effect of this is that the mask length is lowered to
-- be 32 or smaller. Working with 'IPv4Range's that have not been
-- normalized does not cause any issues for this library, although
-- other applications may reject such ranges (especially those with
-- a mask length above 32).
--
-- Note that 'normalize' is idempotent, that is:
--
-- prop> normalize r == (normalize . normalize) r
normalize :: IPv4Range -> IPv4Range
normalize (IPv4Range (IPv4 w) len) =
  let len' = min len 32
      w' = w .&. Internal.mask len'
   in IPv4Range (IPv4 w') len'


-- | This only exists for doctests. Do not use it.
prRange :: IPv4Range -> IO ()
prRange (IPv4Range (IPv4 addr) range) =
  Text.putStrLn (Internal.rangeToDotDecimalText addr range)

