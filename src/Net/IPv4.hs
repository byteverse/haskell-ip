{-# LANGUAGE BangPatterns #-}

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
    -- * Textual Conversion
    -- ** Text
  , encode
  , decode
  , decodeEither
  , builder
  , reader
  , parser
  , print
    -- ** UTF-8 ByteString
  , encodeUtf8
  , decodeUtf8
  , builderUtf8
  , parserUtf8
    -- ** String
    -- $string
  , encodeString
  , decodeString
  ) where

import Prelude hiding (any,print)
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
import Data.Vector.Generic.Mutable (MVector(..))
import Control.Monad.Primitive (PrimMonad,PrimState)
import Foreign.Ptr (Ptr,plusPtr)
import Foreign.Storable (poke)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8')
import qualified Data.ByteString.Builder as Builder
import qualified Net.Internal as Internal
import qualified Data.Text.Lazy as LText
import qualified Data.Text.IO as Text
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Text.Read as Text (Reader)
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString
import qualified Data.Text as Text
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Unsafe as BSU

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

-- | The broadcast IP address: @255.255.255.255@
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
               || Internal.mask8  .&. w == c
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

encode :: IPv4 -> Text
encode = Internal.toDotDecimalText . getIPv4

decodeEither :: Text -> Either String IPv4
decodeEither = coerce . Internal.decodeIPv4TextEither

decode :: Text -> Maybe IPv4
decode = Internal.rightToMaybe . decodeEither

builder :: IPv4 -> TBuilder.Builder
builder = Internal.toDotDecimalBuilder . getIPv4

reader :: Text.Reader IPv4
reader = coerce Internal.decodeIPv4TextReader

parser :: AT.Parser IPv4
parser = coerce Internal.dotDecimalParser

-- | This exists mostly for testing purposes.
print :: IPv4 -> IO ()
print = Text.putStrLn . encode

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
parserUtf8 = coerce $ Internal.fromOctets'
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
      then fail "All octets in an ip address must be between 0 and 255"
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


twoDigits :: ByteString
twoDigits = BC8.pack
  "0001020304050607080910111213141516171819\
  \2021222324252627282930313233343536373839\
  \4041424344454647484950515253545556575859\
  \6061626364656667686970717273747576777879\
  \8081828384858687888990919293949596979899"

threeDigits :: ByteString
threeDigits =
  Data.ByteString.replicate 300 0 <> BC8.pack
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
