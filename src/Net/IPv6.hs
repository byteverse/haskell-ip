{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}

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
  , decodeShort
  , parser
    -- * UTF-8 Bytes
  , parserUtf8Bytes
  , decodeUtf8Bytes
  , boundedBuilderUtf8
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

import Prelude hiding (any, print)

import Net.IPv4 (IPv4(..))

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad (mzero)
import Control.Monad.ST (ST)
import Data.Bits
import Data.Char (chr)
import Data.Data (Data)
import Data.Ix (Ix)
import Data.List (intercalate, group)
import Data.Primitive (MutablePrimArray)
import Data.Primitive.Types (Prim)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.WideWord.Word128 (Word128(..), zeroWord128)
import Data.Word
import Foreign.Storable (Storable)
import GHC.Exts (Int#,Word#,Int(I#))
import GHC.Generics (Generic)
import GHC.Word (Word16(W16#))
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec (prec,step)
import Text.Read (Read(..),Lexeme(Ident),lexP,parens)

import qualified Arithmetic.Lte as Lte
import qualified Arithmetic.Nat as Nat
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteArray.Builder.Bounded as BB
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as Parser
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.ByteString.Short.Internal as BSS
import qualified Data.Primitive as PM
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Text.Short.Unsafe as TS
import qualified Data.Text.Short as TS
import qualified Net.IPv4 as IPv4

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
  deriving (Bounded,Enum,Eq,Integral,Num,Ord,Real,Storable,Bits,FiniteBits,NFData,Prim,Ix,Data)

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

-- | Decode 'ShortText' as an 'IPv6' address.
--
--   >>> decodeShort "ffff::2:b"
--   Just (ipv6 0xffff 0x0000 0x0000 0x0000 0x0000 0x0000 0x0002 0x000b)
decodeShort :: ShortText -> Maybe IPv6
decodeShort t = decodeUtf8Bytes (Bytes.fromByteArray b)
  where b = shortByteStringToByteArray (TS.toShortByteString t)

shortByteStringToByteArray :: BSS.ShortByteString -> PM.ByteArray
shortByteStringToByteArray (BSS.SBS x) = PM.ByteArray x

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
--
-- Per <https://tools.ietf.org/html/rfc5952#section-4.2.2 Section 4.2.2> of the
-- same RFC, this does not use @::@ to shorten a single 16-bit 0 field. Only
-- runs of multiple 0 fields are considered.
encode :: IPv6 -> Text
encode !ip =
  -- TODO: This implementation, while correct, is not particularly efficient.
  -- It uses string all over the place.
  if isIPv4Mapped ip
    -- This representation is RECOMMENDED by https://tools.ietf.org/html/rfc5952#section-5
    then
      Text.pack "::ffff:"
      `mappend`
      IPv4.encode (IPv4.IPv4 (fromIntegral w7 `unsafeShiftL` 16 .|. fromIntegral w8))
    else toText [w1, w2, w3, w4, w5, w6, w7, w8]
  where
  (w1, w2, w3, w4, w5, w6, w7, w8) = toWord16s ip
  toText ws = Text.pack $ intercalate ":"
      $ expand 0 (if longestZ > 1 then longestZ else 0) grouped
    where
    expand !_ 8 !_ = ["::"]
    expand !_ !_ [] = []
    expand !i !longest ((x, len):wsNext)
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

isIPv4Mapped :: IPv6 -> Bool
isIPv4Mapped (IPv6 (Word128 w1 w2)) =
  w1 == 0 && (0xFFFFFFFF00000000 .&. w2 == 0x0000FFFF00000000)

-- | Decode UTF-8-encoded 'Bytes' into an 'IPv6' address.
--
--   >>> decodeUtf8Bytes (Bytes.fromAsciiString "::cab:1")
--   Just (ipv6 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0cab 0x0001)
decodeUtf8Bytes :: Bytes.Bytes -> Maybe IPv6
decodeUtf8Bytes !b = case Parser.parseBytes (parserUtf8Bytes ()) b of
  Parser.Success (Parser.Slice _ len addr) -> case len of
    0 -> Just addr
    _ -> Nothing
  Parser.Failure _ -> Nothing

-- | Encodes the 'IPv6' address using zero-compression on the
-- leftmost longest string of zeroes in the address.
--
-- >>> BB.run Nat.constant $ boundedBuilderUtf8 $ fromWord16s 0xDEAD 0xBEEF 0x0 0x0 0x0 0x0 0x0 0x1234
-- [0x64, 0x65, 0x61, 0x64, 0x3a, 0x62, 0x65, 0x65, 0x66, 0x3a, 0x3a, 0x31, 0x32, 0x33, 0x34]
boundedBuilderUtf8 :: IPv6 -> BB.Builder 39
boundedBuilderUtf8 !ip@(IPv6 (Word128 hi lo))
  | hi == 0 && lo == 0 = BB.weaken Lte.constant
      (BB.ascii ':' `BB.append` BB.ascii ':')
  | isIPv4Mapped ip = BB.weaken Lte.constant $
      BB.ascii ':'
      `BB.append`
      BB.ascii ':'
      `BB.append`
      BB.ascii 'f'
      `BB.append`
      BB.ascii 'f'
      `BB.append`
      BB.ascii 'f'
      `BB.append`
      BB.ascii 'f'
      `BB.append`
      BB.ascii ':'
      `BB.append`
      IPv4.boundedBuilderUtf8 (IPv4.IPv4 (fromIntegral lo))
  | otherwise =
      let (w0,w1,w2,w3,w4,w5,w6,w7) = toWord16s ip
          IntTriple startLongest longest _ = longestRun w0 w1 w2 w3 w4 w5 w6 w7
          start = startLongest
          end = start + longest
          -- start is inclusive. end is exclusive
       in firstPiece w0 start
          `BB.append`
          piece 1 w1 start end
          `BB.append`
          piece 2 w2 start end
          `BB.append`
          piece 3 w3 start end
          `BB.append`
          piece 4 w4 start end
          `BB.append`
          piece 5 w5 start end
          `BB.append`
          piece 6 w6 start end
          `BB.append`
          lastPiece w7 end

firstPiece :: Word16 -> Int -> BB.Builder 4
firstPiece !w !start = case start of
  0 -> BB.weaken Lte.constant (BB.ascii ':')
  _ -> BB.word16LowerHex w

-- Note about the implementation of piece:
-- It is important to manually perform worker-wrapper so that
-- we can stop piece from inlining. If we do not do this, GHC
-- inlines piece, leading to enormous blowup in the generated
-- Core. The implementation of boundedBuilderUtf8 becomes
-- thousands of lines of Core. Even in the microbenchmark that
-- comes with this library, it can be observed that preventing
-- this inlining improves performance of encodeShort by 50%.
piece :: Int -> Word16 -> Int -> Int -> BB.Builder 5
{-# inline piece #-}
piece (I# ix) (W16# w) (I# start) (I# end) =
  piece# ix w start end

piece# :: Int# -> Word# -> Int# -> Int# -> BB.Builder 5
{-# noinline piece# #-}
piece# !ix# !w# !start# !end# = case compare ix start of
  LT -> BB.ascii ':' `BB.append` BB.word16LowerHex w
  EQ -> BB.weaken Lte.constant (BB.ascii ':')
  GT -> if ix < end
    then BB.weaken Lte.constant BB.empty
    else BB.ascii ':' `BB.append` BB.word16LowerHex w
  where
  ix = I# ix#
  start = I# start#
  end = I# end#
  w = W16# w#

lastPiece :: Word16 -> Int -> BB.Builder 5
lastPiece !w !end = case end of
  8 -> BB.weaken Lte.constant (BB.ascii ':')
  _ -> BB.ascii ':' `BB.append` BB.word16LowerHex w

data IntTriple = IntTriple !Int !Int !Int

-- Choose the longest run. Prefer the leftmost run in the
-- event of a tie.
stepZeroRunLength :: Int -> Word16 -> IntTriple -> IntTriple
stepZeroRunLength !ix !w (IntTriple startLongest longest current) = case w of
  0 -> let !x = current + 1 in
    if x > longest
      then IntTriple (ix - current) x x
      else IntTriple startLongest longest x
  _ -> IntTriple startLongest longest 0

-- We start out by setting the longest run to size 1. This
-- means that we will only detect runs of length two or greater.
longestRun ::
     Word16
  -> Word16
  -> Word16
  -> Word16
  -> Word16
  -> Word16
  -> Word16
  -> Word16
  -> IntTriple
longestRun !w0 !w1 !w2 !w3 !w4 !w5 !w6 !w7 = id
  $ stepZeroRunLength 7 w7
  $ stepZeroRunLength 6 w6
  $ stepZeroRunLength 5 w5
  $ stepZeroRunLength 4 w4
  $ stepZeroRunLength 3 w3
  $ stepZeroRunLength 2 w2
  $ stepZeroRunLength 1 w1
  $ stepZeroRunLength 0 w0
  $ IntTriple (-1) 1 0

-- | Encodes the 'IPv6' address as 'ShortText' using zero-compression on
-- the leftmost longest string of zeroes in the address.
-- Per <https://tools.ietf.org/html/rfc5952#section-5 RFC 5952 Section 5>,
-- this uses mixed notation when encoding an IPv4-mapped IPv6 address.
-- 
-- >>> encodeShort $ fromWord16s 0xDEAD 0xBEEF 0x0 0x0 0x0 0x0ABC 0x0 0x1234
-- "dead:beef::abc:0:1234"
encodeShort :: IPv6 -> ShortText
encodeShort w = id
  $ TS.fromShortByteStringUnsafe
  $ byteArrayToShortByteString
  $ BB.run Nat.constant
  $ boundedBuilderUtf8
  $ w

byteArrayToShortByteString :: PM.ByteArray -> BSS.ShortByteString
byteArrayToShortByteString (PM.ByteArray x) = BSS.SBS x

-- | Decode an 'IPv6' address. This accepts both standard IPv6
-- notation (with zero compression) and mixed notation for
-- IPv4-mapped IPv6 addresses. For a decoding function that
-- additionally accepts dot-decimal-encoded IPv4 addresses,
-- see @Net.IP.decode@.
decode :: Text -> Maybe IPv6
decode t = rightToMaybe (AT.parseOnly (parser <* AT.endOfInput) t)

-- | Parse UTF-8-encoded 'Bytes' as an 'IPv6' address. This accepts
-- both uppercase and lowercase characters in the hexadecimal components.
--
-- >>> let str = "dead:beef:3240:a426:ba68:1cd0:4263:109b -> alive"
-- >>> Parser.parseBytes (parserUtf8Bytes ()) (Bytes.fromAsciiString str)
-- Success (Slice {offset = 39, length = 9, value = ipv6 0xdead 0xbeef 0x3240 0xa426 0xba68 0x1cd0 0x4263 0x109b})
--
-- This does not currently support parsing embedded IPv4 address
-- (e.g. @ff00:8000:abc::224.1.2.3@).
parserUtf8Bytes :: e -> Parser.Parser e s IPv6
parserUtf8Bytes e = do
  marr <- Parser.effect (PM.newPrimArray 8)
  -- We cannot immidiately call preZeroes since it wants a
  -- leading colon present.
  Latin.trySatisfy (== ':') >>= \case
    True -> do
      Latin.char e ':'
      postZeroesBegin e marr 0 0
    False -> do
      w <- pieceParser e
      Parser.effect (PM.writePrimArray marr 0 w)
      preZeroes e marr 1

-- This is called when we are positioned before a colon.
-- We may encounter another colon immidiately after
-- the one that we consume here. This indicates zero
-- compression. Or we may encounter another hex-encoded
-- number.
preZeroes ::
     e
  -> MutablePrimArray s Word16 -- length must be 8
  -> Int
  -> Parser.Parser e s IPv6
preZeroes e !marr !ix = case ix of
  8 -> Parser.effect (combinePieces marr)
  _ -> do
    Latin.char e ':'
    Latin.trySatisfy (== ':') >>= \case
      True -> postZeroesBegin e marr ix ix
      False -> do
        w <- pieceParser e
        Parser.effect (PM.writePrimArray marr ix w)
        preZeroes e marr (ix + 1)

-- The same as postZeroes except that there is no
-- leading that gets consumed. This is called right
-- after a double colon is consumed.
-- Precondition: the index is less than 8. This parser
-- is only called by preZeroes, which ensures that
-- this holds.
postZeroesBegin ::
     e
  -> MutablePrimArray s Word16 -- length must be 8
  -> Int -- current index in array
  -> Int -- index where compression happened
  -> Parser.Parser e s IPv6
postZeroesBegin e !marr !ix !compress = do
  optionalPieceParser e >>= \case
    Nothing -> do -- the end has come
      Parser.effect (conclude marr ix compress)
    Just w -> do
      Parser.effect (PM.writePrimArray marr ix w)
      postZeroes e marr (ix + 1) compress

-- Should be run right before a colon.
postZeroes :: 
     e
  -> MutablePrimArray s Word16 -- length must be 8
  -> Int -- current index in array
  -> Int -- index where compression happened
  -> Parser.Parser e s IPv6
postZeroes e !marr !ix !compress = case ix of
  8 -> Parser.fail e
  _ -> do
    Latin.trySatisfy (== ':') >>= \case
      False -> -- The end has come
        Parser.effect (conclude marr ix compress)
      True -> do
        w <- pieceParser e
        Parser.effect (PM.writePrimArray marr ix w)
        postZeroes e marr (ix + 1) compress

conclude :: MutablePrimArray s Word16 -> Int -> Int -> ST s IPv6
conclude !marr !ix !compress = do
  -- This will overlap, but GHC's copy primop is fine with that.
  let postCompressionLen = ix - compress
  PM.copyMutablePrimArray marr (8 - postCompressionLen) marr compress postCompressionLen
  let compressedArea = 8 - ix
  PM.setPrimArray marr compress compressedArea (0 :: Word16)
  combinePieces marr

-- Example memmove that may need to happen:
-- A B C H  ==> A B C 0 0 0 0 H
--       *
-- ix = 4, compress = 3, postCompressionLen = 1, compressedArea = 4
-- copyPrimArray marr 7 marr 3 1
-- setPrimArray marr 3 4 (0 :: Word16)

combinePieces ::
     MutablePrimArray s Word16
  -> ST s IPv6
combinePieces !marr = fromWord16s
  <$> PM.readPrimArray marr 0
  <*> PM.readPrimArray marr 1
  <*> PM.readPrimArray marr 2
  <*> PM.readPrimArray marr 3
  <*> PM.readPrimArray marr 4
  <*> PM.readPrimArray marr 5
  <*> PM.readPrimArray marr 6
  <*> PM.readPrimArray marr 7

optionalPieceParser :: e -> Parser.Parser e s (Maybe Word16)
optionalPieceParser e = Latin.tryHexNibble >>= \case
  Nothing -> pure Nothing
  Just w0 -> do
    r <- pieceParserStep e w0
    pure (Just r)

pieceParser :: e -> Parser.Parser e s Word16
pieceParser e = Latin.hexNibble e >>= pieceParserStep e

-- Parses the remainder of a lowercase hexadecimal number.
-- Leaves trailing colons alone. This fails if there are
-- more than four hex digits unless there are leading zeroes.
-- I cannot find a spec that is clear about what to do
-- if someone puts 00000 in a piece of an encoded IPv6
-- address, so I veer on the side of leniency.
pieceParserStep ::
     e
  -> Word
  -> Parser.Parser e s Word16
pieceParserStep e !acc = if acc > 0xFFFF
  then Parser.fail e
  else Latin.tryHexNibble >>= \case
    Nothing -> pure (fromIntegral acc)
    Just w -> pieceParserStep e (16 * acc + w)

-- | Parse an 'IPv6' using 'Atto.Parser'.
--
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
  } deriving (Eq,Ord,Show,Read,Generic,Data)

instance NFData IPv6Range

instance Aeson.ToJSON IPv6Range where
  toJSON = Aeson.String . encodeRange

instance Aeson.FromJSON IPv6Range where
  parseJSON (Aeson.String t) = case decodeRange t of
    Nothing -> fail "Could not decodeRange IPv6 range"
    Just res -> return res
  parseJSON _ = mzero

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
