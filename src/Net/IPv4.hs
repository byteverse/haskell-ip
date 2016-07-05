{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  ( -- * Types
    IPv4(..)
  , IPv4Range(..)
    -- * Range functions
  , mask
  , normalize
  , member
  , lowerInclusive
  , upperInclusive
    -- * Private Ranges
  , private24
  , private20
  , private16
    -- * Conversion Functions
  , fromOctets
  , fromOctets'
  , toOctets
    -- * Internal Functions
    -- $internal
  , prAddr
  , prRange
  , fromDotDecimalText
  , fromDotDecimalText'
  , rangeFromDotDecimalText'
  , dotDecimalRangeParser
  , dotDecimalParser
  , toDotDecimalText
  , toDotDecimalBuilder
  , rangeToDotDecimalText
  , rangeToDotDecimalBuilder
  ) where

import qualified Data.Text.Lazy         as LText
import qualified Data.Text.IO           as Text
import qualified Data.Text.Lazy.Builder as TBuilder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Monoid ((<>))
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)
import Data.Word
import Data.Int
import Data.Hashable
import Data.Aeson (FromJSON(..),ToJSON(..))
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.Text as AT
import Net.Internal (attoparsecParseJSON,rightToMaybe)
import Control.Monad
import Data.Text.Internal (Text(..))
import Control.Monad.ST
import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)
import Data.ByteString (ByteString)
import Data.Vector.Generic.Mutable      (MVector(..))
import Control.Monad.Primitive          (PrimMonad,PrimState)
import qualified Data.Vector.Unboxed    as UVector
import qualified Data.ByteString.Char8  as BC8
import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Array        as TArray

-- $setup
--
-- These are here to get doctest's property checking to work
--
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> instance Arbitrary IPv4 where { arbitrary = fmap IPv4 arbitrary }
-- >>> instance Arbitrary IPv4Range where { arbitrary = IPv4Range <$> arbitrary <*> arbitrary }
--

-- | A 32-bit Internet Protocol address.
newtype IPv4 = IPv4 { getIPv4 :: Word32 }
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Hashable,Generic)

-- | The length should be between 0 and 32. These bounds are inclusive.
--   This expectation is not in any way enforced by this library because
--   it does not cause errors. A mask length greater than 32 will be
--   treated as if it were 32.
data IPv4Range = IPv4Range
  { ipv4RangeBase   :: {-# UNPACK #-} !IPv4
  , ipv4RangeLength :: {-# UNPACK #-} !Word8
  } deriving (Eq,Ord,Show,Read,Generic)

instance Hashable IPv4Range

instance ToJSON IPv4 where
  toJSON addr = Aeson.String (toDotDecimalText addr)

instance FromJSON IPv4 where
  parseJSON = attoparsecParseJSON (dotDecimalParser <* AT.endOfInput)

instance ToJSON IPv4Range where
  toJSON addrRange = Aeson.String (rangeToDotDecimalText addrRange)

instance FromJSON IPv4Range where
  parseJSON (Aeson.String t) =
    case rangeFromDotDecimalText' t of
      Left err  -> fail err
      Right res -> return res
  parseJSON _ = mzero

newtype instance UVector.MVector s IPv4 = MV_IPv4 (UVector.MVector s Word32)

instance MVector UVector.MVector IPv4 where
  basicLength = coerce (basicLength :: UVector.MVector s Word32 -> Int)
  basicUnsafeSlice = coerce (basicUnsafeSlice :: Int -> Int -> UVector.MVector s Word32 -> UVector.MVector s Word32)
  basicInitialize :: forall m. PrimMonad m => UVector.MVector (PrimState m) IPv4 -> m ()
  basicInitialize = coerce (basicInitialize :: PrimMonad m => UVector.MVector (PrimState m) Word32 -> m ())
  basicUnsafeReplicate :: forall m. PrimMonad m => Int -> IPv4 -> m (UVector.MVector (PrimState m) IPv4)
  basicUnsafeReplicate i (IPv4 w) = fmap coerce (basicUnsafeReplicate i w :: m (UVector.MVector (PrimState m) Word32))
  basicUnsafeRead :: forall m. PrimMonad m => UVector.MVector (PrimState m) IPv4 -> Int -> m IPv4
  basicUnsafeRead v i = fmap coerce (basicUnsafeRead (coerce v :: UVector.MVector (PrimState m) Word32) i :: m Word32)
  basicUnsafeWrite :: forall m. PrimMonad m => UVector.MVector (PrimState m) IPv4 -> Int -> IPv4 -> m ()
  basicUnsafeWrite = coerce (basicUnsafeWrite :: UVector.MVector (PrimState m) Word32 -> Int -> Word32 -> m ())
  basicClear :: forall m. PrimMonad m => UVector.MVector (PrimState m) IPv4 -> m ()
  basicClear = coerce (basicClear :: UVector.MVector (PrimState m) Word32 -> m ())
  basicSet :: forall m. PrimMonad m => UVector.MVector (PrimState m) IPv4 -> IPv4 -> m ()
  basicSet = coerce (basicSet :: UVector.MVector (PrimState m) Word32 -> Word32 -> m ())
  basicUnsafeCopy :: forall m. PrimMonad m => UVector.MVector (PrimState m) IPv4 -> UVector.MVector (PrimState m) IPv4 -> m ()
  basicUnsafeCopy = coerce (basicUnsafeCopy :: UVector.MVector (PrimState m) Word32 -> UVector.MVector (PrimState m) Word32 -> m ())
  basicUnsafeMove :: forall m. PrimMonad m => UVector.MVector (PrimState m) IPv4 -> UVector.MVector (PrimState m) IPv4 -> m ()
  basicUnsafeMove = coerce (basicUnsafeMove :: UVector.MVector (PrimState m) Word32 -> UVector.MVector (PrimState m) Word32 -> m ())
  basicUnsafeGrow :: forall m. PrimMonad m => UVector.MVector (PrimState m) IPv4 -> Int -> m (UVector.MVector (PrimState m) IPv4)
  basicUnsafeGrow (MV_IPv4 v) i = fmap coerce (basicUnsafeGrow v i)

mask :: Word8 -> Word32
mask = complement . shiftR 0xffffffff . fromIntegral

-- normalizeInternal :: Word8 -> Word32 -> Word32
-- normalizeInternal len w = w .&. mask len

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
      w' = w .&. mask len'
   in IPv4Range (IPv4 w') len'

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
  let theMask = mask len
      wsubnetNormalized = wsubnet .&. theMask
   in \(IPv4 w) -> (w .&. theMask) == wsubnetNormalized

-- | This is provided to mirror the interface provided by @Data.Set@. It
-- behaves just like 'contains' but with flipped arguments.
--
-- prop> member ip r == contains r ip
member :: IPv4 -> IPv4Range -> Bool
member = flip contains

lowerInclusive :: IPv4Range -> IPv4
lowerInclusive (IPv4Range (IPv4 w) len) =
  IPv4 (w .&. mask len)

upperInclusive :: IPv4Range -> IPv4
upperInclusive (IPv4Range (IPv4 w) len) =
  let theInvertedMask = shiftR 0xffffffff (fromIntegral len)
      theMask = complement theInvertedMask
   in IPv4 ((w .&. theMask) .|. theInvertedMask)

-- | The RFC1918 24-bit block. Subnet mask: @10.0.0.0/8@
private24 :: IPv4Range
private24 = IPv4Range (fromOctets 10 0 0 0) 8

-- | The RFC1918 20-bit block. Subnet mask: @172.16.0.0/12@
private20 :: IPv4Range
private20  = IPv4Range (fromOctets 172 16 0 0) 12

-- | The RFC1918 16-bit block. Subnet mask: @192.168.0.0/16@
private16 :: IPv4Range
private16 = IPv4Range (fromOctets 192 168 0 0) 16

fromDotDecimalText' :: Text -> Either String IPv4
fromDotDecimalText' t =
  AT.parseOnly (dotDecimalParser <* AT.endOfInput) t

fromDotDecimalText :: Text -> Maybe IPv4
fromDotDecimalText = rightToMaybe . fromDotDecimalText'

rangeFromDotDecimalText' :: Text -> Either String IPv4Range
rangeFromDotDecimalText' t =
  AT.parseOnly (dotDecimalRangeParser <* AT.endOfInput) t

rangeFromDotDecimalText :: Text -> Maybe IPv4Range
rangeFromDotDecimalText = rightToMaybe . rangeFromDotDecimalText'

dotDecimalRangeParser :: AT.Parser IPv4Range
dotDecimalRangeParser = IPv4Range
  <$> dotDecimalParser
  <*  AT.char '/'
  <*> (AT.decimal >>= limitSize)
  where
  limitSize i =
    if i > 32
      then fail "An IP range length must be between 0 and 32"
      else return i

-- | This does not do an endOfInput check because it is
-- reused in the range parser implementation.
dotDecimalParser :: AT.Parser IPv4
dotDecimalParser = fromOctets'
  <$> (AT.decimal >>= limitSize)
  <*  AT.char '.'
  <*> (AT.decimal >>= limitSize)
  <*  AT.char '.'
  <*> (AT.decimal >>= limitSize)
  <*  AT.char '.'
  <*> (AT.decimal >>= limitSize)
  where
  limitSize i =
    if i > 255
      then fail "All octets in an ip address must be between 0 and 255"
      else return i

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
fromOctets a b c d = fromOctets'
  (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)

-- | This is sort of a misnomer. It takes Word32 to make
--   dotDecimalParser probably perform better. This is mostly
--   for internal use.
fromOctets' :: Word32 -> Word32 -> Word32 -> Word32 -> IPv4
fromOctets' a b c d = IPv4
    ( shiftL a 24
  .|. shiftL b 16
  .|. shiftL c 8
  .|. d
    )

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

-- | $internal
-- Everything below here is not part of the stable API. Many of these
-- functions must live here because they are needed for the 'ToJSON' and
-- 'FromJSON' instances. Hopefully, at some point, these can be removed
-- from this module.

-- | This only exists for doctests. Do not use it.
prAddr :: IPv4 -> IO ()
prAddr = Text.putStrLn . toDotDecimalText

-- | This only exists for doctests. Do not use it.
prRange :: IPv4Range -> IO ()
prRange = Text.putStrLn . rangeToDotDecimalText

toDotDecimalText :: IPv4 -> Text
toDotDecimalText = toTextPreAllocated
{-# INLINE toDotDecimalText #-}

toDotDecimalBuilder :: IPv4 -> TBuilder.Builder
toDotDecimalBuilder = TBuilder.fromText . toTextPreAllocated
{-# INLINE toDotDecimalBuilder #-}

rangeToDotDecimalText :: IPv4Range -> Text
rangeToDotDecimalText = LText.toStrict . TBuilder.toLazyText . rangeToDotDecimalBuilder

rangeToDotDecimalBuilder :: IPv4Range -> TBuilder.Builder
rangeToDotDecimalBuilder (IPv4Range addr len) =
  toDotDecimalBuilder addr
  <> TBuilder.singleton '/'
  <> decimal len

-- | I think that this function can be improved. Right now, it
--   always allocates enough space for a fifteen-character text
--   rendering of an IP address. I think that it should be possible
--   to do more of the math upfront and allocate less space.
toTextPreAllocated :: IPv4 -> Text
toTextPreAllocated (IPv4 w) =
  let w1 = fromIntegral $ 255 .&. shiftR w 24
      w2 = fromIntegral $ 255 .&. shiftR w 16
      w3 = fromIntegral $ 255 .&. shiftR w 8
      w4 = fromIntegral $ 255 .&. w
      dot = 46
      (arr,len) = runST $ do
        marr <- TArray.new 15
        i1 <- putAndCount 0 w1 marr
        let n1 = i1
            n1' = i1 + 1
        TArray.unsafeWrite marr n1 dot
        i2 <- putAndCount n1' w2 marr
        let n2 = i2 + n1'
            n2' = n2 + 1
        TArray.unsafeWrite marr n2 dot
        i3 <- putAndCount n2' w3 marr
        let n3 = i3 + n2'
            n3' = n3 + 1
        TArray.unsafeWrite marr n3 dot
        i4 <- putAndCount n3' w4 marr
        theArr <- TArray.unsafeFreeze marr
        return (theArr,i4 + n3')
  in Text arr 0 len

putAndCount :: Int -> Word8 -> TArray.MArray s -> ST s Int
putAndCount pos w marr
  | w < 10 = TArray.unsafeWrite marr pos (i2w w) >> return 1
  | w < 100 = write2 pos w >> return 2
  | otherwise = write3 pos w >> return 3
  where
  write2 off i0 = do
    let i = fromIntegral i0; j = i + i
    TArray.unsafeWrite marr off $ get2 j
    TArray.unsafeWrite marr (off + 1) $ get2 (j + 1)
  write3 off i0 = do
    let i = fromIntegral i0; j = i + i + i
    TArray.unsafeWrite marr off $ get3 j
    TArray.unsafeWrite marr (off + 1) $ get3 (j + 1)
    TArray.unsafeWrite marr (off + 2) $ get3 (j + 2)
  get2 = fromIntegral . ByteString.unsafeIndex twoDigits
  get3 = fromIntegral . ByteString.unsafeIndex threeDigits

zero :: Word16
zero = 48
{-# INLINE zero #-}

i2w :: (Integral a) => a -> Word16
i2w v = zero + fromIntegral v
{-# INLINE i2w #-}

twoDigits :: ByteString
twoDigits = BC8.pack
  "0001020304050607080910111213141516171819\
  \2021222324252627282930313233343536373839\
  \4041424344454647484950515253545556575859\
  \6061626364656667686970717273747576777879\
  \8081828384858687888990919293949596979899"

threeDigits :: ByteString
threeDigits =
  ByteString.replicate 300 0 <> BC8.pack
  "100101102103104105106107108109110111112\
  \113114115116117118119120121122123124125\
  \126127128129130131132133134135136137138\
  \139140141142143144145146147148149150151\
  \152153154155156157158159160161162163164\
  \165166167168169170171172173174175176177\
  \178179180181182183184185186187188189190\
  \191192193194195196197198199200201202203\
  \204205206207208209210211212213214215216\
  \217218219220221222223224225226227228229\
  \230231232233234235236237238239240241242\
  \243244245246247248249250251252253254255"

