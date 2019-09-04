{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UnboxedTuples              #-}

{-| This module provides the IPv6 data type and functions for working
    with it.
-}
module Net.IPv6
  ( -- * Convert
    ipv6
  , fromOctets
  , fromWord16s
  , fromWord32s
  , fromTupleWord16s
  , fromTupleWord32s
  , toWord16s
  , toWord32s
    -- * Special IP Addresses
  , any
  , loopback
  , localhost
    -- * Textual Conversion
    -- ** Text
  , encode
  , encodeShort
  , decode
  , parser
    -- ** Printing
  , print
    -- * IPv6 Ranges
    -- ** Range functions
  , range
  , fromBounds
  , normalize
  , contains
  , member
  , lowerInclusive
  , upperInclusive
    -- ** Textual Conversion
    -- *** Text
  , encodeRange
  , decodeRange
  , parserRange
  , printRange
    -- * Types
  , IPv6(..)
  , IPv6Range(..)
  ) where

import Net.IPv4 (IPv4(..))
import qualified Net.IPv4 as IPv4

import Control.Applicative
import Control.DeepSeq (NFData)
import Data.Bits
import Data.Char (chr)
import Data.List (intercalate, group)
import Data.Primitive.Types (Prim)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.Text as Atto
import Data.Text (Text)
import Data.Text.Short (ShortText)
import qualified Data.Text as Text
import qualified Data.Text.Short as TS
import qualified Data.Text.IO as TIO
import Data.WideWord.Word128 (Word128(..), zeroWord128)
import Data.Word
import Foreign.Storable (Storable)
import GHC.Exts
import GHC.Generics (Generic)
import Numeric (showHex)
import Prelude hiding (any, print)
import Text.ParserCombinators.ReadPrec (prec,step)
import Text.Read (Read(..),Lexeme(Ident),lexP,parens)

-- $setup
--
-- These are here to get doctest work.
--
-- >>> import qualified Prelude as P
-- >>> import qualified Data.Text.IO as T
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> instance Arbitrary Word128 where { arbitrary = Word128 <$> arbitrary <*> arbitrary }
-- >>> instance Arbitrary IPv6 where { arbitrary = IPv6 <$> arbitrary }
-- >>> instance Arbitrary IPv6Range where { arbitrary = IPv6Range <$> arbitrary <*> arbitrary }
--

-- | A 128-bit Internet Protocol version 6 address.
newtype IPv6 = IPv6 { getIPv6 :: Word128 }
  deriving (Bounded,Enum,Eq,Integral,Num,Ord,Real,Storable,Bits,FiniteBits,NFData,Prim)

instance Show IPv6 where
  showsPrec p addr = showParen (p > 10)
    $ showString "ipv6 "
    . showHexWord16 a
    . showChar ' '
    . showHexWord16 b
    . showChar ' '
    . showHexWord16 c
    . showChar ' '
    . showHexWord16 d
    . showChar ' '
    . showHexWord16 e
    . showChar ' '
    . showHexWord16 f
    . showChar ' '
    . showHexWord16 g
    . showChar ' '
    . showHexWord16 h
    where
    (a,b,c,d,e,f,g,h) = toWord16s addr

-- | Print an 'IPv6' using the textual encoding.
print :: IPv6 -> IO ()
print = TIO.putStrLn . encode

showHexWord16 :: Word16 -> ShowS
showHexWord16 w =
    showString "0x"
  . showChar (nibbleToHex (unsafeShiftR (fromIntegral w) 12))
  . showChar (nibbleToHex ((unsafeShiftR (fromIntegral w) 8) .&. 0xF))
  . showChar (nibbleToHex ((unsafeShiftR (fromIntegral w) 4) .&. 0xF))
  . showChar (nibbleToHex ((fromIntegral w) .&. 0xF))

-- invariant: argument must be less than 16
nibbleToHex :: Word -> Char
nibbleToHex w
  | w < 10 = chr (fromIntegral (w + 48))
  | otherwise = chr (fromIntegral (w + 87))

instance Read IPv6 where
  readPrec = parens $ prec 10 $ do
    Ident "ipv6" <- lexP
    a <- step readPrec
    b <- step readPrec
    c <- step readPrec
    d <- step readPrec
    e <- step readPrec
    f <- step readPrec
    g <- step readPrec
    h <- step readPrec
    return (fromWord16s a b c d e f g h)

instance Aeson.ToJSON IPv6 where
  toJSON = Aeson.String . encode

instance Aeson.FromJSON IPv6 where
  parseJSON = Aeson.withText "IPv6" $ \t -> case decode t of
    Nothing -> fail "invalid IPv6 address"
    Just i  -> return i

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

-- | This could be useful for the rare occasion
--   in which one could construct an 'IPv6' from
--   octets.
--
--   Note that while @Net.IPv4.'Net.IPv4.fromOctets' = Net.IPv4.'Net.IPv4.ipv4'@,
--   @Net.IPv6.fromOctets /= Net.IPv6.ipv6@. While this should be obvious
--   from their types, it is worth mentioning since the similarity in naming
--   might be confusing.
fromOctets ::
     Word8 -> Word8 -> Word8 -> Word8
  -> Word8 -> Word8 -> Word8 -> Word8
  -> Word8 -> Word8 -> Word8 -> Word8
  -> Word8 -> Word8 -> Word8 -> Word8
  -> IPv6
fromOctets a b c d e f g h i j k l m n o p =
  IPv6 $ fromOctetsWord128
    (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)
    (fromIntegral e) (fromIntegral f) (fromIntegral g) (fromIntegral h)
    (fromIntegral i) (fromIntegral j) (fromIntegral k) (fromIntegral l)
    (fromIntegral m) (fromIntegral n) (fromIntegral o) (fromIntegral p)

fromOctetsWord128 ::
     Word128 -> Word128 -> Word128 -> Word128
  -> Word128 -> Word128 -> Word128 -> Word128
  -> Word128 -> Word128 -> Word128 -> Word128
  -> Word128 -> Word128 -> Word128 -> Word128
  -> Word128
fromOctetsWord128 a b c d e f g h i j k l m n o p = fromIntegral
    ( shiftL a 120
  .|. shiftL b 112
  .|. shiftL c 104
  .|. shiftL d 96
  .|. shiftL e 88
  .|. shiftL f 80
  .|. shiftL g 72
  .|. shiftL h 64
  .|. shiftL i 56
  .|. shiftL j 48
  .|. shiftL k 40
  .|. shiftL l 32
  .|. shiftL m 24
  .|. shiftL n 16
  .|. shiftL o 8
  .|. p
    )

-- | Create an 'IPv6' address from the eight 16-bit fragments that make
--   it up. This closely resembles the standard IPv6 notation, so
--   is used for the 'Show' instance. Note that this lacks the formatting
--   feature for suppress zeroes in an 'IPv6' address, but it should be
--   readable enough for hacking in GHCi.
--
--   >>> let addr = ipv6 0x3124 0x0 0x0 0xDEAD 0xCAFE 0xFF 0xFE00 0x1
--   >>> addr
--   ipv6 0x3124 0x0000 0x0000 0xdead 0xcafe 0x00ff 0xfe00 0x0001
--   >>> T.putStrLn (encode addr)
--   3124::dead:cafe:ff:fe00:1
ipv6 ::
     Word16 -> Word16 -> Word16 -> Word16
  -> Word16 -> Word16 -> Word16 -> Word16
  -> IPv6
ipv6 = fromWord16s

-- | An alias for the 'ipv6' smart constructor.
fromWord16s ::
     Word16 -> Word16 -> Word16 -> Word16
  -> Word16 -> Word16 -> Word16 -> Word16
  -> IPv6
fromWord16s a b c d e f g h =
  IPv6 $ fromWord16sWord128
    (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)
    (fromIntegral e) (fromIntegral f) (fromIntegral g) (fromIntegral h)

fromWord16sWord128 ::
     Word128 -> Word128 -> Word128 -> Word128
  -> Word128 -> Word128 -> Word128 -> Word128
  -> Word128
fromWord16sWord128 a b c d e f g h = fromIntegral
    ( shiftL a 112
  .|. shiftL b 96
  .|. shiftL c 80
  .|. shiftL d 64
  .|. shiftL e 48
  .|. shiftL f 32
  .|. shiftL g 16
  .|. h
    )

-- | Convert an 'IPv6' to eight 16-bit words.
toWord16s :: IPv6 -> (Word16,Word16,Word16,Word16,Word16,Word16,Word16,Word16)
toWord16s (IPv6 (Word128 a b)) =
  -- Note: implementing this as 2 Word64 shifts with 'unsafeShiftR'
  -- is up to 40% faster than using 128-bit shifts on a Word128 value.
  ( fromIntegral (unsafeShiftR a 48)
  , fromIntegral (unsafeShiftR a 32)
  , fromIntegral (unsafeShiftR a 16)
  , fromIntegral a
  , fromIntegral (unsafeShiftR b 48)
  , fromIntegral (unsafeShiftR b 32)
  , fromIntegral (unsafeShiftR b 16)
  , fromIntegral b
  )

-- | Uncurried variant of 'fromWord16s'.
fromTupleWord16s :: (Word16,Word16,Word16,Word16,Word16,Word16,Word16,Word16) -> IPv6
fromTupleWord16s (a,b,c,d,e,f,g,h) = fromWord16s a b c d e f g h

-- | Build an 'IPv6' from four 32-bit words. The leftmost argument
--   is the high word and the rightword is the low word.
fromWord32s :: Word32 -> Word32 -> Word32 -> Word32 -> IPv6
fromWord32s a b c d =
  IPv6 $ fromWord32sWord128
    (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)

fromWord32sWord128 ::
     Word128 -> Word128 -> Word128 -> Word128
  -> Word128
fromWord32sWord128 a b c d = fromIntegral
    ( shiftL a 96
  .|. shiftL b 64
  .|. shiftL c 32
  .|. d
    )

-- | Uncurried variant of 'fromWord32s'.
fromTupleWord32s :: (Word32,Word32,Word32,Word32) -> IPv6
fromTupleWord32s (a,b,c,d) = fromWord32s a b c d

-- | Convert an 'IPv6' to four 32-bit words.
toWord32s :: IPv6 -> (Word32,Word32,Word32,Word32)
toWord32s (IPv6 (Word128 a b)) =
  -- Note: implementing this as 2 Word64 shifts with 'unsafeShiftR'
  -- is about 10% faster than using 128-bit shifts on a Word128 value.
  ( fromIntegral (unsafeShiftR a 32)
  , fromIntegral a
  , fromIntegral (unsafeShiftR b 32)
  , fromIntegral b
  )

-- | The local loopback IP address.
--
--   >>> loopback
--   ipv6 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0001
loopback :: IPv6
loopback = IPv6 (Word128 0 1)

-- | A useful alias for 'loopback'.
--
--   >>> localhost
--   ipv6 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0001
localhost :: IPv6
localhost = loopback

-- | The IP address representing any host.
--
--   >>> any
--   ipv6 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000
any :: IPv6
any = IPv6 zeroWord128

-- | Encodes the 'IPv6' address using zero-compression on the leftmost longest
-- string of zeroes in the address.
-- Per <https://tools.ietf.org/html/rfc5952#section-5 RFC 5952 Section 5>,
-- this uses mixed notation when encoding an IPv4-mapped IPv6 address:
--
-- >>> T.putStrLn $ encode $ fromWord16s 0xDEAD 0xBEEF 0x0 0x0 0x0 0x0 0x0 0x1234
-- dead:beef::1234
-- >>> T.putStrLn $ encode $ fromWord16s 0x0 0x0 0x0 0x0 0x0 0xFFFF 0x6437 0xA5B4
-- ::ffff:100.55.165.180
-- >>> T.putStrLn $ encode $ fromWord16s 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0
-- ::
encode :: IPv6 -> Text
encode ip =
  -- TODO: This implementation, while correct, is not particularly efficient.
  -- It uses string all over the place.
  if isIPv4MappedAddress
  -- This representation is RECOMMENDED by https://tools.ietf.org/html/rfc5952#section-5
  then Text.pack "::ffff:" `mappend` IPv4.encode (IPv4.IPv4 (fromIntegral w7 `unsafeShiftL` 16 .|. fromIntegral w8))
  else toText [w1, w2, w3, w4, w5, w6, w7, w8]
  where
  isIPv4MappedAddress = w1 == 0 && w2 == 0 && w3 == 0 && w4 == 0 && w5 == 0 && w6 == 0xFFFF
  (w1, w2, w3, w4, w5, w6, w7, w8) = toWord16s ip
  toText ws = Text.pack $ intercalate ":" $ expand 0 longestZ grouped
    where
    expand _ 8 _ = ["::"]
    expand _ _ [] = []
    expand i longest ((x, len):wsNext)
        -- zero-compressed group:
        | x == 0 && len == longest =
            -- first and last need an extra colon since there's nothing
            -- to concat against
            (if i == 0 || (i+len) == 8 then ":" else "")
            : expand (i+len) 0 wsNext
        -- normal group:
        | otherwise = replicate len (showHex x "") ++ expand (i+len) longest wsNext
    longestZ = maximum . (0:) . map snd . filter ((==0) . fst) $ grouped
    grouped = map (\x -> (head x, length x)) (group ws)

-- | Encodes the 'IPv6' address as 'ShortText' using zero-compression on
-- the leftmost longest string of zeroes in the address.
-- Per <https://tools.ietf.org/html/rfc5952#section-5 RFC 5952 Section 5>,
-- this uses mixed notation when encoding an IPv4-mapped IPv6 address.
encodeShort :: IPv6 -> ShortText
encodeShort = TS.fromText . encode
-- This (encodeShort) should be rewritten to not go through
-- UTF-16-encoded text as an intermediary.

-- | Decode an 'IPv6' address. This accepts both standard IPv6
-- notation (with zero compression) and mixed notation for
-- IPv4-mapped IPv6 addresses. For a decoding function that
-- additionally accepts dot-decimal-encoded IPv4 addresses,
-- see @Net.IP.decode@.
decode :: Text -> Maybe IPv6
decode t = rightToMaybe (AT.parseOnly (parser <* AT.endOfInput) t)

-- | Parse an 'IPv6' using 'Atto.Parser'.
--
--   >>> ip = ipv6 0xDEAD 0xBEEF 0x3240 0xA426 0xBA68 0x1CD0 0x4263 0x109B
--   >>> Atto.parseOnly parser (Text.pack "dead:beef:3240:a426:ba68:1cd0:4263:109b")
--   Right (ipv6 0xdead 0xbeef 0x3240 0xa426 0xba68 0x1cd0 0x4263 0x109b)
parser :: Atto.Parser IPv6
parser = makeIP <$> ip
  where
  makeIP [w1, w2, w3, w4, w5, w6, w7, w8] = fromWord16s w1 w2 w3 w4 w5 w6 w7 w8
  makeIP _ = error "Net.IPv6.parser: Implementation error. Please open a bug report."

  ip = (Atto.char ':' *> Atto.char ':' *> doubleColon 0) <|> part 0

  part :: Int -> Atto.Parser [Word16]
  part n =
    case n of
      -- max 8 parts in an IPv6 address
      7 -> pure <$> Atto.hexadecimal
      -- after 6 parts it could end in IPv4 dotted notation
      6 -> ipv4 <|> hexPart
      _ -> hexPart
    where
    hexPart = (:)
      <$> Atto.hexadecimal
      <*> (Atto.char ':' *>
            (
             (Atto.char ':' *> doubleColon (n+1))
             <|>
             part (n+1)
            )
          )

  doubleColon :: Int -> Atto.Parser [Word16]
  doubleColon count = do
    rest <- afterDoubleColon <|> pure []
    let fillerLength = (8 - count - length rest)
    if fillerLength <= 0
      then fail "too many parts in IPv6 address"
      else pure (replicate fillerLength 0 ++ rest)

  -- after double colon, IPv4 dotted notation could appear anywhere
  afterDoubleColon :: Atto.Parser [Word16]
  afterDoubleColon =
    ipv4 <|>
    (:) <$> Atto.hexadecimal <*> ((Atto.char ':' *> afterDoubleColon) <|> pure [])

  ipv4 :: Atto.Parser [Word16]
  ipv4 = ipv4ToWord16s <$> IPv4.parser

  ipv4ToWord16s :: IPv4 -> [Word16]
  ipv4ToWord16s (IPv4 word) = [fromIntegral (word `unsafeShiftR` 16), fromIntegral (word .&. 0xFFFF)]

-- | An 'IPv6Range'. It is made up of the first 'IPv6' in the range
--   and its length.
data IPv6Range = IPv6Range
  { ipv6RangeBase   :: {-# UNPACK #-} !IPv6
  , ipv6RangeLength :: {-# UNPACK #-} !Word8
  } deriving (Eq,Ord,Show,Read,Generic)

instance NFData IPv6Range

mask128 :: IPv6
mask128 = maxBound

mask :: Word8 -> IPv6
mask = complement . shiftR mask128 . fromIntegral

-- | Normalize an 'IPv6Range'. The first result of this is that the
--   'IPv6' inside the 'IPv6Range' is changed so that the insignificant
--   bits are zeroed out. For example:
--
--   >>> addr1 = ipv6 0x0192 0x0168 0x0001 0x0019 0x0000 0x0000 0x0000 0x0000
--   >>> addr2 = ipv6 0x0192 0x0168 0x0001 0x0163 0x0000 0x0000 0x0000 0x0000
--   >>> printRange $ normalize $ IPv6Range addr1 24
--   192:100::/24
--   >>> printRange $ normalize $ IPv6Range addr2 28
--   192:160::/28
--
--   The second effect of this is that the mask length is lowered to be 128
--   or smaller. Working with 'IPv6Range's that have not been normalized does
--   not cause any issues for this library, although other applications may
--   reject such ranges (especially those with a mask length above 128).
--
--   Note that 'normalize is idempotent, that is:
--
--   prop> normalize r == (normalize . normalize) r
normalize :: IPv6Range -> IPv6Range
normalize (IPv6Range ip len) =
  let len' = min len 128
      ip' = ip .&. mask len'
  in IPv6Range ip' len'

-- | Encode an 'IPv6Range' as 'Text'.
--
--   >>> addr = ipv6 0xDEAD 0xBEEF 0x3240 0xA426 0xBA68 0x1CD0 0x4263 0x109B
--   >>> T.putStrLn $ encodeRange $ IPv6Range addr 28
--   dead:beef:3240:a426:ba68:1cd0:4263:109b/28
encodeRange :: IPv6Range -> Text
encodeRange x = encode (ipv6RangeBase x) <> Text.pack "/" <> (Text.pack $ (show . fromEnum) $ ipv6RangeLength x)

-- | Decode an 'IPv6Range' from 'Text'.
--
--   >>> addr = ipv6 0xDEAD 0xBEEF 0x3240 0xA426 0xBA68 0x1CD0 0x4263 0x109B
--   >>> fmap encodeRange $ decodeRange (Text.pack "dead:beef:3240:a426:ba68:1cd0:4263:109b/28")
--   Just "dead:bee0::/28"
decodeRange :: Text -> Maybe IPv6Range
decodeRange = rightToMaybe . AT.parseOnly (parserRange <* AT.endOfInput)

-- | Parse an 'IPv6Range' using a 'AT.Parser'.
parserRange :: AT.Parser IPv6Range
parserRange = do
  ip <- parser
  _ <- AT.char '/'
  theMask <- AT.decimal >>= limitSize
  return (normalize (IPv6Range ip theMask))
  where
  limitSize i =
    if i > 128
      then fail "An IP range length must be between 0 and 128"
      else return i

-- | Checks to see if an 'IPv6' address belongs in the 'IPv6Range'.
--
-- >>> let ip = ipv6 0x2001 0x0db8 0x0db8 0x1094 0x2051 0x0000 0x0000 0x0001
-- >>> let iprange mask = IPv6Range (ipv6 0x2001 0x0db8 0x0000 0x0000 0x0000 0x0000 0x0000 0x0001) mask
-- >>> contains (iprange 8) ip
-- True
-- >>> contains (iprange 48) ip
-- False
--
-- Typically, element-testing functions are written to take the element
-- as the first argument and the set as the second argument. This is intentionally
-- written the other way for better performance when iterating over a collection.
-- For example, you might test elements in a list for membership like this:
--
-- >>> let r = IPv6Range (ipv6 0x2001 0x0db8 0x0000 0x0000 0x0000 0x0000 0x0000 0x0001) 64
-- >>> fmap (contains r) (take 5 $ iterate succ $ ipv6 0x2001 0x0db8 0x0000 0x0000 0xffff 0xffff 0xffff 0xfffe)
-- [True,True,False,False,False]
--
-- The implementation of 'contains' ensures that (with GHC), the bitmask
-- creation and range normalization only occur once in the above example.
-- They are reused as the list is iterated.
contains :: IPv6Range -> IPv6 -> Bool
contains (IPv6Range subnet len) =
  let theMask = mask len
      subnetNormalized = subnet .&. theMask
   in \ip -> (ip .&. theMask) == subnetNormalized

-- | This is provided to mirror the interface provided by @Data.Set@. It
-- behaves just like 'contains' but with flipped arguments.
--
-- prop> member ip r == contains r ip
member :: IPv6 -> IPv6Range -> Bool
member = flip contains

-- | The inclusive lower bound of an 'IPv6Range'. This is conventionally
--   understood to be the broadcast address of a subnet. For example:
--
-- >>> T.putStrLn $ encode $ lowerInclusive $ IPv6Range (ipv6 0x2001 0x0db8 0x0000 0x0000 0x0000 0x0000 0x0000 0x0001) 25
-- 2001:d80::
--
-- Note that the lower bound of a normalized 'IPv6Range' is simply the
-- ip address of the range:
--
-- prop> lowerInclusive r == ipv6RangeBase (normalize r)
lowerInclusive :: IPv6Range -> IPv6
lowerInclusive = ipv6RangeBase . normalize

-- | The inclusive upper bound of an 'IPv6Range'.
--
--   >>> let addr = ipv6 0xDEAD 0xBEEF 0x3240 0xA426 0xBA68 0x1CD0 0x4263 0x109B
--   >>> T.putStrLn $ encode $ upperInclusive $ IPv6Range addr 25
--   dead:beff:ffff:ffff:ffff:ffff:ffff:ffff
--
upperInclusive :: IPv6Range -> IPv6
upperInclusive (IPv6Range ip len) =
  let len' = min 128 len
      theInvertedMask :: IPv6
      theInvertedMask = shiftR mask128 (fromIntegral len')
  in ip .|. theInvertedMask

-- | Print an 'IPv6Range' using the textual encoding.
printRange :: IPv6Range -> IO ()
printRange = TIO.putStrLn . encodeRange

-- | Smart constructor for 'IPv6Range'. Ensures the mask is appropriately
--   sized and sets masked bits in the 'IPv6' to zero.
--
--   >>> let addr = ipv6 0xDEAD 0xBEEF 0x3240 0xA426 0xBA68 0x1CD0 0x4263 0x109B
--   >>> printRange $ range addr 25
--   dead:be80::/25
range :: IPv6 -> Word8 -> IPv6Range
range addr len = normalize (IPv6Range addr len)

-- | Given an inclusive lower and upper ip address, create the smallest 'IPv6Range'
--   that contains the two. This is helpful in situations where input is given as a
--   range, like @ @.
--
--   This makes the range broader if it cannot be represented in <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing CIDR> notation.
--
--   >>> addrLower = ipv6 0xDEAD 0xBE80 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000
--   >>> addrUpper = ipv6 0xDEAD 0xBEFF 0xFFFF 0xFFFF 0xFFFF 0xFFFF 0xFFFF 0xFFFF
--   >>> printRange $ fromBounds addrLower addrUpper
--   dead:be80::/25
fromBounds :: IPv6 -> IPv6 -> IPv6Range
fromBounds lo hi =
  normalize (IPv6Range lo (maskFromBounds lo hi))

maskFromBounds :: IPv6 -> IPv6 -> Word8
maskFromBounds lo hi = fromIntegral (countLeadingZeros $ xor lo hi)
