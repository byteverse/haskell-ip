{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wall #-}
{-| An IPv4 data type

    This module provides the IPv4 data type and functions for working
    with it. There are also encoding and decoding functions provided
    in this module, but they should be imported from
    @Net.IPv4.Text@ and @Net.ByteString.Char8@ instead. They are
    defined here so that the 'FromJSON' and 'ToJSON' instances can
    use them.

    At some point, a highly efficient IPv4-to-ByteString function needs
    to be added to this module to take advantage of @aeson@'s new
    @toEncoding@ method.
-}

module Net.IPv4
  ( -- * Conversion Functions
    ipv4
  , fromOctets
  , fromTupleOctets
  , toOctets
    -- * Special IP Addresses
  , any
  , loopback
  , broadcast
    -- * Range Predicates
  , private
  , reserved
  , public
    -- * Textual Conversion
    -- ** Text
  , encode
  , decode
  , builder
  , reader
  , parser
    -- ** UTF-8 ByteString
  , encodeUtf8
  , decodeUtf8
  , builderUtf8
  , parserUtf8
    -- ** String
    -- $string
  , encodeString
  , decodeString
    -- ** Printing
  , print
    -- * IPv4 Ranges
    -- ** Range functions
  , range
  , fromBounds
  , normalize
  , contains
  , member
  , lowerInclusive
  , upperInclusive
    -- ** Conversion to IPv4
  , toList
  , toGenerator
    -- ** Private Ranges
  , private24
  , private20
  , private16
    -- ** Textual Conversion
    -- *** Text
  , encodeRange
  , decodeRange
  , rangeBuilder
  , rangeParser
  , rangePrint
    -- * Types
  , IPv4(..)
  , IPv4Range(..)
    -- * Interoperability
    -- $interoperability
  ) where

import Control.Monad
import Control.Monad.ST (ST,runST)
import Data.Aeson (FromJSON(..),ToJSON(..))
import Data.Bits ((.&.),(.|.),shiftR,shiftL,unsafeShiftR,complement,shift)
import Data.ByteString (ByteString)
import Data.Hashable
import Data.Monoid ((<>))
import Data.Primitive.Types (Prim)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Internal (Text(..))
import Data.Vector.Generic.Mutable (MVector(..))
import Data.Word
import Foreign.Ptr (Ptr,plusPtr)
import Foreign.Storable (Storable, poke)
import GHC.Generics (Generic)
import Prelude hiding (any, print, print)
import Text.ParserCombinators.ReadPrec (prec,step)
import Text.Printf (printf)
import Text.Read (Read(..),Lexeme(Ident),lexP,parens)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.Attoparsec.Text as AT
import qualified Data.Bits as Bits
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Array as TArray
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Lazy.Builder.Int as TBI
import qualified Data.Text.Read as TextRead
import qualified Data.Vector.Generic as GVector
import qualified Data.Vector.Generic.Mutable as MGVector
import qualified Data.Vector.Primitive as PVector
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Unboxed.Mutable as MUVector

#if MIN_VERSION_aeson(1,0,0)
import Data.Aeson (ToJSONKey(..),FromJSONKey(..),
  ToJSONKeyFunction(..),FromJSONKeyFunction(..))
#endif

-- $setup
--
-- These are here to get doctest's property checking to work
--
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> import qualified Prelude as P
-- >>> import qualified Data.Text.IO as T
-- >>> instance Arbitrary IPv4 where { arbitrary = fmap IPv4 arbitrary }
--

-- | Create an 'IPv4' address from four octets. The first argument
--   is the most significant octet. The last argument is the least
--   significant. Since IP addresses are commonly written using dot-decimal
--   notation, this is the recommended way to create an IP address.
--   Additionally, it is used for the 'Show' and 'Read' instances
--   of 'IPv4' to help keep things readable in GHCi.
--
--   >>> let addr = ipv4 192 168 1 1
--   >>> addr
--   ipv4 192 168 1 1
--   >>> getIPv4 addr
--   3232235777
--
ipv4 :: Word8 -> Word8 -> Word8 -> Word8 -> IPv4
ipv4 = fromOctets

-- | An alias for the 'ipv4' smart constructor.
fromOctets :: Word8 -> Word8 -> Word8 -> Word8 -> IPv4
fromOctets a b c d = fromOctets'
  (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)

-- | An uncurried variant of 'fromOctets'.
fromTupleOctets :: (Word8,Word8,Word8,Word8) -> IPv4
fromTupleOctets (a,b,c,d) = fromOctets a b c d

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

-- | The local loopback IP address: @127.0.0.1@
loopback :: IPv4
loopback = fromOctets 127 0 0 1

-- | The broadcast IP address: @255.255.255.255@
broadcast :: IPv4
broadcast = fromOctets 255 255 255 255

-- | Checks to see if the 'IPv4' address belongs to a private
-- network. The three private networks that are checked are
-- @10.0.0.0/8@, @172.16.0.0/12@, and @192.168.0.0/16@.
private :: IPv4 -> Bool
private (IPv4 w) =
     mask8  .&. w == p24
  || mask12 .&. w == p20
  || mask16 .&. w == p16

-- | Checks to see if the 'IPv4' address belongs to a reserved
-- network. This includes the three private networks that 'private'
-- checks along with several other ranges that are not used
-- on the public Internet.
reserved :: IPv4 -> Bool
reserved =
  let a = getIPv4 $ fromOctets' 0 0 0 0
      b = getIPv4 $ fromOctets' 100 64 0 0
      c = getIPv4 $ fromOctets' 127 0 0 0
      d = getIPv4 $ fromOctets' 169 254 0 0
      e = getIPv4 $ fromOctets' 192 0 0 0
      f = getIPv4 $ fromOctets' 192 0 2 0
      g = getIPv4 $ fromOctets' 192 88 99 0
      h = getIPv4 $ fromOctets' 198 18 0 0
      i = getIPv4 $ fromOctets' 198 51 100 0
      j = getIPv4 $ fromOctets' 203 0 113 0
      k = getIPv4 $ fromOctets' 224 0 0 0
      l = getIPv4 $ fromOctets' 240 0 0 0
      m = getIPv4 $ fromOctets' 255 255 255 255
  in \(IPv4 w) -> mask8  .&. w == p24
               || mask12 .&. w == p20
               || mask16 .&. w == p16
               || mask8  .&. w == a
               || mask10 .&. w == b
               || mask8  .&. w == c
               || mask16 .&. w == d
               || mask24 .&. w == e
               || mask24 .&. w == f
               || mask24 .&. w == g
               || mask15 .&. w == h
               || mask24 .&. w == i
               || mask24 .&. w == j
               || mask4  .&. w == k
               || mask4  .&. w == l
               || mask32 .&. w == m

mask8,mask4,mask12,mask16,mask10,mask24,mask32,mask15 :: Word32
mask4  = 0xF0000000
mask8  = 0xFF000000
mask10 = 0xFFC00000
mask12 = 0xFFF00000
mask15 = 0xFFFE0000
mask16 = 0xFFFF0000
mask24 = 0xFFFFFF00
mask32 = 0xFFFFFFFF

-- | Checks to see if the 'IPv4' address is publicly routable.
--
-- prop> public x == not (reserved x)
public :: IPv4 -> Bool
public = not . reserved

-- | Encode an 'IPv4' address to 'Text' using dot-decimal notation:
--
--   >>> T.putStrLn (encode (ipv4 192 168 2 47))
--   192.168.2.47
encode :: IPv4 -> Text
encode = toDotDecimalText

-- | Decode an 'IPv4' address.
decode :: Text -> Maybe IPv4
decode = decodeIPv4TextMaybe

-- | Encode an 'IPv4' address to a text 'TBuilder.Builder'.
builder :: IPv4 -> TBuilder.Builder
builder = toDotDecimalBuilder

reader :: TextRead.Reader IPv4
reader = decodeIPv4TextReader

parser :: AT.Parser IPv4
parser = dotDecimalParser

-- | Encode an 'IPv4' address to a UTF-8 encoded 'ByteString'.
encodeUtf8 :: IPv4 -> ByteString
encodeUtf8 = toBSPreAllocated

toBSPreAllocated :: IPv4 -> ByteString
toBSPreAllocated (IPv4 !w) = I.unsafeCreateUptoN 15 (\ptr1 ->
  do len1 <- writeWord ptr1 w1
     let ptr2 = ptr1 `plusPtr` len1
     poke ptr2 dot
     len2 <- writeWord (ptr2 `plusPtr` 1) w2
     let ptr3 = ptr2 `plusPtr` len2 `plusPtr` 1
     poke ptr3 dot
     len3 <- writeWord (ptr3 `plusPtr` 1) w3
     let ptr4 = ptr3 `plusPtr` len3 `plusPtr` 1
     poke ptr4 dot
     len4 <- writeWord (ptr4 `plusPtr` 1) w4
     return (3 + len1 + len2 + len3 + len4))
  where w1 = fromIntegral $ shiftR w 24
        w2 = fromIntegral $ shiftR w 16
        w3 = fromIntegral $ shiftR w 8
        w4 = fromIntegral w
        dot = 46 :: Word8
        writeWord :: Ptr Word8 -> Word8 -> IO Int
        writeWord !ptr !word
          | word >= 100 = do
              let int = fromIntegral word
                  indx = int + int + int
                  get3 = fromIntegral . ByteString.unsafeIndex threeDigits
              poke ptr (get3 indx)
              poke (ptr `plusPtr` 1) (get3 (indx + 1))
              poke (ptr `plusPtr` 2) (get3 (indx + 2))
              return 3
          | word >= 10 = do
              let int = fromIntegral word
                  indx = int + int
                  get2 = fromIntegral . ByteString.unsafeIndex twoDigits
              poke ptr (get2 indx)
              poke (ptr `plusPtr` 1) (get2 (indx + 1))
              return 2
          | otherwise = do
              poke ptr (word + 48)
              return 1

-- This should be rewritten to not go through text
-- as an intermediary.
decodeUtf8 :: ByteString -> Maybe IPv4
decodeUtf8 = decode <=< rightToMaybe . decodeUtf8'

builderUtf8 :: IPv4 -> Builder.Builder
builderUtf8 = Builder.byteString . encodeUtf8

parserUtf8 :: AB.Parser IPv4
parserUtf8 = fromOctets'
  <$> (AB.decimal >>= limitSize)
  <*  AB.char '.'
  <*> (AB.decimal >>= limitSize)
  <*  AB.char '.'
  <*> (AB.decimal >>= limitSize)
  <*  AB.char '.'
  <*> (AB.decimal >>= limitSize)
  where
  limitSize i =
    if i > 255
      then fail "All octets in an ipv4 address must be between 0 and 255"
      else return i

{- $string

    These functions exist for the convenience of those who need a
    'String' representation of an 'IPv4' address. Using them
    is discouraged unless the end user is working with a library
    that can only use 'String' to deal with textual data (such as
    @pandoc@, @hxr@, or @network@).

-}

encodeString :: IPv4 -> String
encodeString = Text.unpack . encode

decodeString :: String -> Maybe IPv4
decodeString = decode . Text.pack


-- | A 32-bit Internet Protocol version 4 address. To use this with the
--   @network@ library, it is necessary to use @Network.Socket.htonl@ to
--   convert the underlying 'Word32' from host byte order to network byte
--   order.
newtype IPv4 = IPv4 { getIPv4 :: Word32 }
  deriving (Eq,Ord,Enum,Bounded,Hashable,Generic,Prim,Storable)

instance Show IPv4 where
  showsPrec p addr = showParen (p > 10)
    $ showString "ipv4 "
    . showsPrec 11 a
    . showChar ' '
    . showsPrec 11 b
    . showChar ' '
    . showsPrec 11 c
    . showChar ' '
    . showsPrec 11 d
    where
    (a,b,c,d) = toOctets addr

instance Read IPv4 where
  readPrec = parens $ prec 10 $ do
    Ident "ipv4" <- lexP
    a <- step readPrec
    b <- step readPrec
    c <- step readPrec
    d <- step readPrec
    return (fromOctets a b c d)

print :: IPv4 -> IO ()
print = TIO.putStrLn . encode

newtype instance UVector.MVector s IPv4 = MV_IPv4 (PVector.MVector s IPv4)
newtype instance UVector.Vector IPv4 = V_IPv4 (PVector.Vector IPv4)

instance UVector.Unbox IPv4

instance MGVector.MVector UVector.MVector IPv4 where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_IPv4 v) = MGVector.basicLength v
  basicUnsafeSlice i n (MV_IPv4 v) = MV_IPv4 $ MGVector.basicUnsafeSlice i n v
  basicOverlaps (MV_IPv4 v1) (MV_IPv4 v2) = MGVector.basicOverlaps v1 v2
  basicUnsafeNew n = MV_IPv4 `liftM` MGVector.basicUnsafeNew n
  basicInitialize (MV_IPv4 v) = MGVector.basicInitialize v
  basicUnsafeReplicate n x = MV_IPv4 `liftM` MGVector.basicUnsafeReplicate n x
  basicUnsafeRead (MV_IPv4 v) i = MGVector.basicUnsafeRead v i
  basicUnsafeWrite (MV_IPv4 v) i x = MGVector.basicUnsafeWrite v i x
  basicClear (MV_IPv4 v) = MGVector.basicClear v
  basicSet (MV_IPv4 v) x = MGVector.basicSet v x
  basicUnsafeCopy (MV_IPv4 v1) (MV_IPv4 v2) = MGVector.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_IPv4 v1) (MV_IPv4 v2) = MGVector.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_IPv4 v) n = MV_IPv4 `liftM` MGVector.basicUnsafeGrow v n

instance GVector.Vector UVector.Vector IPv4 where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_IPv4 v) = V_IPv4 `liftM` GVector.basicUnsafeFreeze v
  basicUnsafeThaw (V_IPv4 v) = MV_IPv4 `liftM` GVector.basicUnsafeThaw v
  basicLength (V_IPv4 v) = GVector.basicLength v
  basicUnsafeSlice i n (V_IPv4 v) = V_IPv4 $ GVector.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_IPv4 v) i = GVector.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_IPv4 mv) (V_IPv4 v) = GVector.basicUnsafeCopy mv v
  elemseq _ = seq

instance ToJSON IPv4 where
  toJSON = Aeson.String . encode

instance FromJSON IPv4 where
  parseJSON = Aeson.withText "IPv4" aesonParser

#if MIN_VERSION_aeson(1,0,0)
instance ToJSONKey IPv4 where
  toJSONKey = ToJSONKeyText
    encode
    (\addr -> Aeson.unsafeToEncoding $ Builder.char7 '"' <> builderUtf8 addr <> Builder.char7 '"')

instance FromJSONKey IPv4 where
  fromJSONKey = FromJSONKeyTextParser aesonParser
#endif

aesonParser :: Text -> Aeson.Parser IPv4
aesonParser t = case decode t of
  Nothing -> fail "Could not parse IPv4 address"
  Just addr -> return addr

ipv4Bitwise :: (Word32 -> Word32 -> Word32) -> IPv4 -> IPv4 -> IPv4
ipv4Bitwise fun l r = IPv4 $ (getIPv4 l) `fun` (getIPv4 r)

-- | Note: we use network order (big endian) as opposed to host order (little
--   endian) which differs from the underlying IPv4 type representation.
instance Bits.Bits IPv4 where
    (.&.) = ipv4Bitwise (.&.)
    (.|.) = ipv4Bitwise (.|.)
    xor = ipv4Bitwise Bits.xor
    complement = IPv4 . Bits.complement . getIPv4
    shift ip i = IPv4 $ Bits.shift (getIPv4 ip) i
    rotate ip i = IPv4 $ Bits.rotate (getIPv4 ip) i
    bitSize = Bits.finiteBitSize
    bitSizeMaybe = Bits.bitSizeMaybe . getIPv4
    isSigned = Bits.isSigned . getIPv4
    testBit ip i = Bits.testBit (getIPv4 ip) $ Bits.finiteBitSize ip - 1 - i
    bit i = IPv4 $ Bits.bit $ Bits.finiteBitSize any - 1 - i
    popCount = Bits.popCount . getIPv4

instance Bits.FiniteBits IPv4 where
    finiteBitSize = Bits.finiteBitSize . getIPv4

------------------------------------
-- Internal functions, not exported
------------------------------------

decodeIPv4TextMaybe :: Text -> Maybe IPv4
decodeIPv4TextMaybe t = case decodeIPv4TextReader t of
  Left _ -> Nothing
  Right (w,t') -> if Text.null t'
    then Just w
    else Nothing

decodeIPv4TextReader :: TextRead.Reader IPv4
decodeIPv4TextReader t1' = do
  (a,t2) <- TextRead.decimal t1'
  t2' <- stripDecimal t2
  (b,t3) <- TextRead.decimal t2'
  t3' <- stripDecimal t3
  (c,t4) <- TextRead.decimal t3'
  t4' <- stripDecimal t4
  (d,t5) <- TextRead.decimal t4'
  if a > 255 || b > 255 || c > 255 || d > 255
    then Left ipOctetSizeErrorMsg
    else Right (fromOctets' a b c d,t5)

stripDecimal :: Text -> Either String Text
stripDecimal t = case Text.uncons t of
  Nothing -> Left "expected a dot but input ended instead"
  Just (c,tnext) -> if c == '.'
    then Right tnext
    else Left "expected a dot but found a different character"

-- | This is sort of a misnomer. It takes Word to make
--   dotDecimalParser perform better. This is mostly
--   for internal use. The arguments must all fit
--   in a Word8.
fromOctets' :: Word -> Word -> Word -> Word -> IPv4
fromOctets' a b c d = IPv4 $ fromIntegral
    ( shiftL a 24
  .|. shiftL b 16
  .|. shiftL c 8
  .|. d
    )

p24 :: Word32
p24 = getIPv4 (fromOctets' 10 0 0 0)

p20 :: Word32
p20 = getIPv4 (fromOctets' 172 16 0 0)

p16 :: Word32
p16 = getIPv4 (fromOctets' 192 168 0 0)

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
      then fail ipOctetSizeErrorMsg
      else return i

ipOctetSizeErrorMsg :: String
ipOctetSizeErrorMsg = "All octets in an IPv4 address must be between 0 and 255"

toDotDecimalText :: IPv4 -> Text
toDotDecimalText = toTextPreAllocated

toDotDecimalBuilder :: IPv4 -> TBuilder.Builder
toDotDecimalBuilder = TBuilder.fromText . toTextPreAllocated

-- | I think that this function can be improved. Right now, it
--   always allocates enough space for a fifteen-character text
--   rendering of an IP address. I think that it should be possible
--   to do more of the math upfront and allocate less space.
toTextPreAllocated :: IPv4 -> Text
toTextPreAllocated (IPv4 w) =
  let w1 = 255 .&. unsafeShiftR (fromIntegral w) 24
      w2 = 255 .&. unsafeShiftR (fromIntegral w) 16
      w3 = 255 .&. unsafeShiftR (fromIntegral w) 8
      w4 = 255 .&. fromIntegral w
   in toTextPreallocatedPartTwo w1 w2 w3 w4

toTextPreallocatedPartTwo :: Word -> Word -> Word -> Word -> Text
toTextPreallocatedPartTwo !w1 !w2 !w3 !w4 =
#ifdef ghcjs_HOST_OS
  let dotStr = "."
   in Text.pack $ concat
        [ show w1
        , "."
        , show w2
        , "."
        , show w3
        , "."
        , show w4
        ]
#else
  let dot = 46
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
#endif

twoDigits :: ByteString
twoDigits = foldMap (BC8.pack . printf "%02d") $ enumFromTo (0 :: Int) 99
{-# NOINLINE twoDigits #-}

threeDigits :: ByteString
threeDigits = foldMap (BC8.pack . printf "%03d") $ enumFromTo (0 :: Int) 999
{-# NOINLINE threeDigits #-}

i2w :: Integral a => a -> Word16
i2w v = zero + fromIntegral v

zero :: Word16
zero = 48

putAndCount :: Int -> Word -> TArray.MArray s -> ST s Int
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

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

{- $interoperability

The @<http://hackage.haskell.org/package/network network>@ library is commonly
used to open sockets and communicate over them. In the @Network.Socket@ module,
it provides a type synonym @HostAddress@ that, like 'IPv4', is used
to represent an IPv4 address. However, while 'IPv4' uses a big-endian representation
for ip addresses, @HostAddress@ has platform dependent endianness.
Consequently, it is necessary to convert between the two as follows:

> import Network.Socket (HostAddress,htonl,ntohl)
>
> toHostAddr :: IPv4 -> HostAddress
> toHostAddr (IPv4 w) = htonl w
>
> fromHostAddr :: HostAddress -> IPv4
> fromHostAddr w = IPv4 (ntohl w)

These functions are not included with this library since it would require
picking up a dependency on @network@.

-}

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

-- | Smart constructor for 'IPv4Range'. Ensures the mask is appropriately
--   sized and sets masked bits in the 'IPv4' to zero.
range :: IPv4 -> Word8 -> IPv4Range
range addr len = normalize (IPv4Range addr len)

-- | Given an inclusive lower and upper ip address, create the smallest
-- 'IPv4Range' that contains the two. This is helpful in situations where
-- input given as a range like @192.168.16.0-192.168.19.255@ needs to be
-- handled. This makes the range broader if it cannot be represented in
-- CIDR notation.
--
-- >>> rangePrint $ fromBounds (fromOctets 192 168 16 0) (fromOctets 192 168 19 255)
-- 192.168.16.0/22
-- >>> rangePrint $ fromBounds (fromOctets 10 0 5 7) (fromOctets 10 0 5 14)
-- 10.0.5.0/28
fromBounds :: IPv4 -> IPv4 -> IPv4Range
fromBounds (IPv4 a) (IPv4 b) =
  normalize (IPv4Range (IPv4 a) (maskFromBounds a b))

maskFromBounds :: Word32 -> Word32 -> Word8
maskFromBounds lo hi = fromIntegral (Bits.countLeadingZeros (Bits.xor lo hi))

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
-- >>> mapM_ (P.rangePrint . contains r) (take 5 $ iterate succ $ fromOctets 10 10 10 5)
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
-- >>> T.putStrLn $ I.encodeRange $ lowerInclusive $ IPv4Range (ipv4 10 10 1 160) 25
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
-- >>> mapM_ (T.putStrLn . I.encodeRange) (toList r)
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
private24 = IPv4Range (fromOctets 10 0 0 0) 8

-- | The RFC1918 20-bit block. Subnet mask: @172.16.0.0/12@
private20 :: IPv4Range
private20  = IPv4Range (fromOctets 172 16 0 0) 12

-- | The RFC1918 16-bit block. Subnet mask: @192.168.0.0/16@
private16 :: IPv4Range
private16 = IPv4Range (fromOctets 192 168 0 0) 16

-- | Normalize an 'IPv4Range'. The first result of this is that the
-- 'IPv4' inside the 'IPv4Range' is changed so that the insignificant
-- bits are zeroed out. For example:
--
-- >>> rangePrint $ normalize $ IPv4Range (fromOctets 192 168 1 19) 24
-- 192.168.1.0/24
-- >>> rangePrint $ normalize $ IPv4Range (fromOctets 192 168 1 163) 28
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

encodeRange :: IPv4Range -> Text
encodeRange = rangeToDotDecimalText

decodeRange :: Text -> Maybe IPv4Range
decodeRange = rightToMaybe . AT.parseOnly (rangeParser <* AT.endOfInput)

rangeBuilder :: IPv4Range -> TBuilder.Builder
rangeBuilder = rangeToDotDecimalBuilder

rangeParser :: AT.Parser IPv4Range
rangeParser = do
  ip <- parser
  _ <- AT.char '/'
  theMask <- AT.decimal >>= limitSize
  return (normalize (IPv4Range ip theMask))
  where
  limitSize i =
    if i > 32
      then fail "An IP range length must be between 0 and 32"
      else return i

-- | This exists mostly for testing purposes.
rangePrint :: IPv4Range -> IO ()
rangePrint = TIO.putStrLn . encodeRange

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
  toJSON = Aeson.String . encodeRange

instance FromJSON IPv4Range where
  parseJSON (Aeson.String t) = case decodeRange t of
    Nothing -> fail "Could not decodeRange IPv4 range"
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

rangeBitwise :: (IPv4 -> IPv4 -> IPv4) -> IPv4Range -> IPv4Range -> IPv4Range
rangeBitwise fun l r = range ip len
  where
    -- Normalise first
    l' = normalize l
    r' = normalize r
    ip = (ipv4RangeBase l') `fun` (ipv4RangeBase r')
    len = maximum [ipv4RangeLength l, ipv4RangeLength r]

rangeRebase :: (IPv4 -> IPv4) -> IPv4Range -> IPv4Range
rangeRebase fun r = range (fun $ ipv4RangeBase r) (ipv4RangeLength r)

-- | Notes:
--
--     * bit operations use network order (big endian),
--
--     * do not operate on host bits,
--
--     * return a normalized range dropping host bits,
--
--     * and "promote operands" by extending the length to the larger of two
--       ranges.
--
instance Bits.Bits IPv4Range where
  (.&.) = rangeBitwise (.&.)
  (.|.) = rangeBitwise (.|.)
  xor = rangeBitwise Bits.xor
  complement = rangeRebase Bits.complement
  shift r i = rangeRebase (flip Bits.shift i) r
  rotate r i = rangeRebase (flip Bits.rotate i) r
  bitSize = Bits.finiteBitSize
  bitSizeMaybe = Just . Bits.finiteBitSize
  isSigned = Bits.isSigned . ipv4RangeBase
  testBit ip i = Bits.testBit (ipv4RangeBase ip) i
  bit i = IPv4Range (Bits.bit i) $ fromIntegral $ i + 1
  popCount = Bits.popCount . ipv4RangeBase . normalize

-- | Note: the size is determined by the range length
instance Bits.FiniteBits IPv4Range where
  finiteBitSize = fromIntegral . ipv4RangeLength

-----------------
-- Internal Stuff
-----------------

rangeToDotDecimalText :: IPv4Range -> Text
rangeToDotDecimalText = LText.toStrict . TBuilder.toLazyText . rangeToDotDecimalBuilder

rangeToDotDecimalBuilder :: IPv4Range -> TBuilder.Builder
rangeToDotDecimalBuilder (IPv4Range addr len) =
     builder addr
  <> TBuilder.singleton '/'
  <> TBI.decimal len

