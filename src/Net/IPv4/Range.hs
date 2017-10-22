{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}
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
    -- * Textual Conversion
    -- ** Text
  , encode
  , decode
  , builder
  , parser
  , print
    -- * Types
  , IPv4Range(..)
  ) where

import Prelude hiding (print)
import Net.IPv4 (IPv4(..))
import Data.Bits ((.&.),(.|.),shiftR,complement,shift)
import Control.Monad
import Data.Text (Text)
import Data.Word (Word8,Word32,Word64)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON(..),ToJSON(..))
import GHC.Generics (Generic)
import Data.Monoid ((<>))
import qualified Net.IPv4 as IPv4
import qualified Data.Text.IO as Text
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Lazy.Builder.Int as TBI
import qualified Data.Vector.Generic as GVector
import qualified Data.Vector.Generic.Mutable as MGVector
import qualified Data.Vector.Unboxed.Mutable as MUVector
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy as LText

-- $setup
--
-- These are here to get doctest's property checking to work.
--
-- >>> import qualified Prelude as P
-- >>> import qualified Net.IPv4 as I
-- >>> import qualified Data.Text.IO as T
-- >>> import Net.IPv4 (fromOctets,ipv4)
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
-- >>> mapM_ (P.print . contains r) (take 5 $ iterate succ $ fromOctets 10 10 10 5)
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
  let theMask = mask len
      wsubnetNormalized = wsubnet .&. theMask
   in \(IPv4 w) -> (w .&. theMask) == wsubnetNormalized

mask :: Word8 -> Word32
mask = complement . shiftR 0xffffffff . fromIntegral

-- | This is provided to mirror the interface provided by @Data.Set@. It
-- behaves just like 'contains' but with flipped arguments.
--
-- prop> member ip r == contains r ip
member :: IPv4 -> IPv4Range -> Bool
member = flip contains

-- | The inclusive lower bound of an 'IPv4Range'. This is conventionally
--   understood to be the broadcast address of a subnet. For example:
--
-- >>> T.putStrLn $ I.encode $ lowerInclusive $ IPv4Range (ipv4 10 10 1 160) 25
-- 10.10.1.128
--
-- Note that the lower bound of a normalized 'IPv4Range' is simply the
-- ip address of the range:
--
-- prop> lowerInclusive r == ipv4RangeBase (normalize r)
lowerInclusive :: IPv4Range -> IPv4
lowerInclusive (IPv4Range (IPv4 w) len) =
  IPv4 (w .&. mask len)

upperInclusive :: IPv4Range -> IPv4
upperInclusive (IPv4Range (IPv4 w) len) =
  let theInvertedMask = shiftR 0xffffffff (fromIntegral len)
      theMask = complement theInvertedMask
   in IPv4 ((w .&. theMask) .|. theInvertedMask)

-- Given the size of the mask, return the total number of ips in the subnet. This
-- only works for IPv4 addresses because an IPv6 subnet can have up to 2^128
-- addresses. Not exported.
countAddrs :: Word8 -> Word64
countAddrs w =
  let amountToShift = if w > 32
        then 0
        else 32 - fromIntegral w
   in shift 1 amountToShift

wordSuccessors :: Word64 -> IPv4 -> [IPv4]
wordSuccessors !w (IPv4 !a) = if w > 0
  then IPv4 a : wordSuccessors (w - 1) (IPv4 (a + 1))
  else []

wordSuccessorsM :: MonadPlus m => Word64 -> IPv4 -> m IPv4
wordSuccessorsM = go where
  go !w (IPv4 !a) = if w > 0
    then mplus (return (IPv4 a)) (go (w - 1) (IPv4 (a + 1)))
    else mzero

-- | Convert an 'IPv4Range' into a list of the 'IPv4' addresses that
--   are in it.
-- >>> let r = IPv4Range (fromOctets 192 168 1 8) 30
-- >>> mapM_ (T.putStrLn . I.encode) (toList r)
-- 192.168.1.8
-- 192.168.1.9
-- 192.168.1.10
-- 192.168.1.11

toList :: IPv4Range -> [IPv4]
toList (IPv4Range ip len) = 
  let totalAddrs = countAddrs len
   in wordSuccessors totalAddrs ip

toGenerator :: MonadPlus m => IPv4Range -> m IPv4
toGenerator (IPv4Range ip len) =  
  let totalAddrs = countAddrs len
   in wordSuccessorsM totalAddrs ip

-- | The RFC1918 24-bit block. Subnet mask: @10.0.0.0/8@
private24 :: IPv4Range
private24 = IPv4Range (IPv4.fromOctets 10 0 0 0) 8

-- | The RFC1918 20-bit block. Subnet mask: @172.16.0.0/12@
private20 :: IPv4Range
private20  = IPv4Range (IPv4.fromOctets 172 16 0 0) 12

-- | The RFC1918 16-bit block. Subnet mask: @192.168.0.0/16@
private16 :: IPv4Range
private16 = IPv4Range (IPv4.fromOctets 192 168 0 0) 16

-- | Normalize an 'IPv4Range'. The first result of this is that the
-- 'IPv4' inside the 'IPv4Range' is changed so that the insignificant
-- bits are zeroed out. For example:
--
-- >>> print $ normalize $ IPv4Range (fromOctets 192 168 1 19) 24
-- 192.168.1.0/24
-- >>> print $ normalize $ IPv4Range (fromOctets 192 168 1 163) 28
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
      w' = w .&. mask len'
   in IPv4Range (IPv4 w') len'

encode :: IPv4Range -> Text
encode = rangeToDotDecimalText

decode :: Text -> Maybe IPv4Range
decode = rightToMaybe . AT.parseOnly (parser <* AT.endOfInput)

builder :: IPv4Range -> TBuilder.Builder
builder = rangeToDotDecimalBuilder

parser :: AT.Parser IPv4Range
parser = do
  ip <- IPv4.parser
  _ <- AT.char '/'
  theMask <- AT.decimal >>= limitSize
  return (normalize (IPv4Range ip theMask))
  where
  limitSize i =
    if i > 32
      then fail "An IP range length must be between 0 and 32"
      else return i

-- | This exists mostly for testing purposes.
print :: IPv4Range -> IO ()
print = Text.putStrLn . encode

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

-- | The length should be between 0 and 32. These bounds are inclusive.
--   This expectation is not in any way enforced by this library because
--   it does not cause errors. A mask length greater than 32 will be
--   treated as if it were 32.
data IPv4Range = IPv4Range
  { ipv4RangeBase   :: {-# UNPACK #-} !IPv4
  , ipv4RangeLength :: {-# UNPACK #-} !Word8
  } deriving (Eq,Ord,Show,Read,Generic)


instance Hashable IPv4Range

instance ToJSON IPv4Range where
  toJSON = Aeson.String . encode

instance FromJSON IPv4Range where
  parseJSON (Aeson.String t) = case decode t of
    Nothing -> fail "Could not decode IPv4 range"
    Just res -> return res
  parseJSON _ = mzero

data instance MUVector.MVector s IPv4Range = MV_IPv4Range
  !(MUVector.MVector s IPv4)
  !(MUVector.MVector s Word8)
data instance UVector.Vector IPv4Range = V_IPv4Range
  !(UVector.Vector IPv4)
  !(UVector.Vector Word8)

instance UVector.Unbox IPv4Range
instance MGVector.MVector MUVector.MVector IPv4Range where
  {-# INLINE basicLength  #-}
  basicLength (MV_IPv4Range as _) = MGVector.basicLength as
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (MV_IPv4Range as bs)
      = MV_IPv4Range (MGVector.basicUnsafeSlice i_ m_ as)
                     (MGVector.basicUnsafeSlice i_ m_ bs)
  {-# INLINE basicOverlaps  #-}
  basicOverlaps (MV_IPv4Range as1 bs1) (MV_IPv4Range as2 bs2)
      = MGVector.basicOverlaps as1 as2
        || MGVector.basicOverlaps bs1 bs2
  {-# INLINE basicUnsafeNew  #-}
  basicUnsafeNew n_
      = do
          as <- MGVector.basicUnsafeNew n_
          bs <- MGVector.basicUnsafeNew n_
          return $ MV_IPv4Range as bs
  {-# INLINE basicInitialize  #-}
  basicInitialize (MV_IPv4Range as bs)
      = do
          MGVector.basicInitialize as
          MGVector.basicInitialize bs
  {-# INLINE basicUnsafeReplicate  #-}
  basicUnsafeReplicate n_ (IPv4Range a b)
      = do
          as <- MGVector.basicUnsafeReplicate n_ a
          bs <- MGVector.basicUnsafeReplicate n_ b
          return (MV_IPv4Range as bs)
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeRead (MV_IPv4Range as bs) i_
      = do
          a <- MGVector.basicUnsafeRead as i_
          b <- MGVector.basicUnsafeRead bs i_
          return (IPv4Range a b)
  {-# INLINE basicUnsafeWrite  #-}
  basicUnsafeWrite (MV_IPv4Range as bs) i_ (IPv4Range a b)
      = do
          MGVector.basicUnsafeWrite as i_ a
          MGVector.basicUnsafeWrite bs i_ b
  {-# INLINE basicClear  #-}
  basicClear (MV_IPv4Range as bs)
      = do
          MGVector.basicClear as
          MGVector.basicClear bs
  {-# INLINE basicSet  #-}
  basicSet (MV_IPv4Range as bs) (IPv4Range a b)
      = do
          MGVector.basicSet as a
          MGVector.basicSet bs b
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_IPv4Range as1 bs1) (MV_IPv4Range as2 bs2)
      = do
          MGVector.basicUnsafeCopy as1 as2
          MGVector.basicUnsafeCopy bs1 bs2
  {-# INLINE basicUnsafeMove  #-}
  basicUnsafeMove (MV_IPv4Range as1 bs1) (MV_IPv4Range as2 bs2)
      = do
          MGVector.basicUnsafeMove as1 as2
          MGVector.basicUnsafeMove bs1 bs2
  {-# INLINE basicUnsafeGrow  #-}
  basicUnsafeGrow (MV_IPv4Range as bs) m_
      = do
          as' <- MGVector.basicUnsafeGrow as m_
          bs' <- MGVector.basicUnsafeGrow bs m_
          return $ MV_IPv4Range as' bs'

instance GVector.Vector UVector.Vector IPv4Range where
  {-# INLINE basicUnsafeFreeze  #-}
  basicUnsafeFreeze (MV_IPv4Range as bs)
      = do
          as' <- GVector.basicUnsafeFreeze as
          bs' <- GVector.basicUnsafeFreeze bs
          return $ V_IPv4Range as' bs'
  {-# INLINE basicUnsafeThaw  #-}
  basicUnsafeThaw (V_IPv4Range as bs)
      = do
          as' <- GVector.basicUnsafeThaw as
          bs' <- GVector.basicUnsafeThaw bs
          return $ MV_IPv4Range as' bs'
  {-# INLINE basicLength  #-}
  basicLength (V_IPv4Range as _) = GVector.basicLength as
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (V_IPv4Range as bs)
      = V_IPv4Range (GVector.basicUnsafeSlice i_ m_ as)
                    (GVector.basicUnsafeSlice i_ m_ bs)
  {-# INLINE basicUnsafeIndexM  #-}
  basicUnsafeIndexM (V_IPv4Range as bs) i_
      = do
          a <- GVector.basicUnsafeIndexM as i_
          b <- GVector.basicUnsafeIndexM bs i_
          return (IPv4Range a b)
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_IPv4Range as1 bs1) (V_IPv4Range as2 bs2)
      = do
          GVector.basicUnsafeCopy as1 as2
          GVector.basicUnsafeCopy bs1 bs2
  {-# INLINE elemseq  #-}
  elemseq _ (IPv4Range a b)
      = GVector.elemseq (undefined :: UVector.Vector a) a
        . GVector.elemseq (undefined :: UVector.Vector b) b

-----------------
-- Internal Stuff
-----------------

rangeToDotDecimalText :: IPv4Range -> Text
rangeToDotDecimalText = LText.toStrict . TBuilder.toLazyText . rangeToDotDecimalBuilder

rangeToDotDecimalBuilder :: IPv4Range -> TBuilder.Builder
rangeToDotDecimalBuilder (IPv4Range addr len) =
     IPv4.builder addr
  <> TBuilder.singleton '/'
  <> TBI.decimal len

