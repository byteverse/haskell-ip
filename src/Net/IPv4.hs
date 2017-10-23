{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wall #-}
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
    ipv4
  , fromOctets
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
    -- * Types
  , IPv4(..)
  ) where

import Prelude hiding (any)
import Data.Bits ((.&.),(.|.),shiftR,shiftL,unsafeShiftR)
import Data.Word
import Data.Hashable
import Data.Aeson (FromJSON(..),ToJSON(..))
import GHC.Generics (Generic)
import Control.Monad
import Data.Text.Internal (Text(..))
import Data.ByteString (ByteString)
import Data.Vector.Generic.Mutable (MVector(..))
import Foreign.Ptr (Ptr,plusPtr)
import Foreign.Storable (poke)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8')
import Foreign.Storable (Storable)
import Data.Bits (Bits,FiniteBits)
import Data.Primitive.Types (Prim)
import Control.Monad.ST (ST,runST)
import Text.Printf (printf)
import Text.Read (Read(..),Lexeme(Ident),lexP,parens)
import Text.ParserCombinators.ReadPrec (prec,step)
import qualified Data.Text.Read as TextRead
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Text.Read as Text (Reader)
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Text as Text
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector.Generic as GVector
import qualified Data.Vector.Generic.Mutable as MGVector
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Primitive as PVector
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Builder as BB
import qualified Data.Text.Array as TArray

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

reader :: Text.Reader IPv4
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
                  get3 = fromIntegral . BSU.unsafeIndex threeDigits
              poke ptr (get3 indx)
              poke (ptr `plusPtr` 1) (get3 (indx + 1))
              poke (ptr `plusPtr` 2) (get3 (indx + 2))
              return 3
          | word >= 10 = do
              let int = fromIntegral word
                  indx = int + int
                  get2 = fromIntegral . BSU.unsafeIndex twoDigits
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


-- | A 32-bit Internet Protocol version 4 address.
newtype IPv4 = IPv4 { getIPv4 :: Word32 }
  deriving (Eq,Ord,Enum,Bounded,Hashable,Generic,Prim,Bits,FiniteBits,Storable)

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
    (\addr -> Aeson.unsafeToEncoding $ BB.char7 '"' <> builderUtf8 addr <> BB.char7 '"')

instance FromJSONKey IPv4 where
  fromJSONKey = FromJSONKeyTextParser aesonParser
#endif

aesonParser :: Text -> Aeson.Parser IPv4
aesonParser t = case decode t of
  Nothing -> fail "Could not parse IPv4 address"
  Just addr -> return addr


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

