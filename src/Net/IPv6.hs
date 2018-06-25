{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

{-# OPTIONS_GHC -Wall #-}

module Net.IPv6
  ( -- * Types
    IPv6(..)
    -- * Convert
  , ipv6
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
    -- * Textual Conversion
    -- ** Text
  , encode
  , decode
  , parser
    -- ** Printing
  , print
  ) where

import Net.IPv4 (IPv4(..))
import qualified Net.IPv4 as IPv4

import Control.Applicative
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Char (chr)
import Data.List (intercalate, group)
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive.Types (Prim(..))
import Data.Text (Text)
import Data.Word
import GHC.Exts
import Numeric (showHex)
import Prelude hiding (any, print)
import Text.ParserCombinators.ReadPrec (prec,step)
import Text.Read (Read(..),Lexeme(Ident),lexP,parens)
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

-- $setup
--
-- These are here to get doctest work.
--
-- >>> import qualified Prelude as P
-- >>> import qualified Data.Text.IO as T
--

-- | A 128-bit Internet Protocol version 6 address.
data IPv6 = IPv6
  { ipv6A :: {-# UNPACK #-} !Word64
  , ipv6B :: {-# UNPACK #-} !Word64
  } deriving (Eq,Ord)

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

instance Prim IPv6 where
  sizeOf# _ = 2# *# sizeOf# (undefined :: Word64)
  alignment# _ = alignment# (undefined :: Word64)
  indexByteArray# arr# i# =
    let i = I# i#
        arr = ByteArray arr#
    in IPv6 (indexByteArray arr (2 * i + 0)) (indexByteArray arr (2 * i + 1))
  readByteArray# :: forall s. () => MutableByteArray# s -> Int# -> State# s -> (# State# s, IPv6 #)
  readByteArray# arr# i# = internal $ do
    let i = I# i#
        arr = MutableByteArray arr#
    a <- readByteArray arr (2 * i + 0) :: ST s Word64
    b <- readByteArray arr (2 * i + 1)
    return (IPv6 a b)
  writeByteArray# :: forall s. () => MutableByteArray# s -> Int# -> IPv6 -> State# s -> State# s
  writeByteArray# arr# i# (IPv6 a b) = internal_ $ do
    let i = I# i#
        arr = MutableByteArray arr#
    writeByteArray arr (2 * i + 0) a
    writeByteArray arr (2 * i + 1) b :: ST s ()
  setByteArray# arr# i# len# ident = go 0#
    where
      go ix# s0 = if isTrue# (ix# <# len#)
        then case writeByteArray# arr# (i# +# ix#) ident s0 of
          s1 -> go (ix# +# 1#) s1
        else s0
  indexOffAddr# :: Addr# -> Int# -> IPv6
  indexOffAddr# addr# i# =
    let i = I# i#
        addr = Addr addr#
    in IPv6 (indexOffAddr addr (2 * i + 0)) (indexOffAddr addr (2 * i + 1))
  readOffAddr# :: forall s. () => Addr# -> Int# -> State# s -> (# State# s, IPv6 #)
  readOffAddr# addr# i# = internal $ do
    let i = I# i#
        addr = Addr addr#
    a <- readOffAddr addr (2 * i + 0) :: ST s Word64
    b <- readOffAddr addr (2 * i + 1)
    return (IPv6 a b)
  writeOffAddr# :: forall s. () => Addr# -> Int# -> IPv6 -> State# s -> State# s
  writeOffAddr# addr# i# (IPv6 a b) = internal_ $ do
    let i = I# i#
        addr = Addr addr#
    writeOffAddr addr (2 * i + 0) a
    writeOffAddr addr (2 * i + 1) b :: ST s ()
  setOffAddr# addr# i# len# ident = go 0#
    where
      go ix# s0 = if isTrue# (ix# <# len#)
        then case writeOffAddr# addr# (i# +# ix#) ident s0 of
          s1 -> go (ix# +# 1#) s1
        else s0

internal_ :: PrimBase m => m () -> State# (PrimState m) -> State# (PrimState m)
internal_ m s = case internal m s of
  (# s', _ #) -> s'

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
    Just i -> return i
        
rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

fromOctets ::
     Word8 -> Word8 -> Word8 -> Word8
  -> Word8 -> Word8 -> Word8 -> Word8
  -> Word8 -> Word8 -> Word8 -> Word8
  -> Word8 -> Word8 -> Word8 -> Word8
  -> IPv6
fromOctets a b c d e f g h i j k l m n o p =
  let !(w1,w2) = fromOctetsV6
        (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)
        (fromIntegral e) (fromIntegral f) (fromIntegral g) (fromIntegral h)
        (fromIntegral i) (fromIntegral j) (fromIntegral k) (fromIntegral l)
        (fromIntegral m) (fromIntegral n) (fromIntegral o) (fromIntegral p)
   in IPv6 w1 w2

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
  let !(w1,w2) = fromWord16sV6
        (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)
        (fromIntegral e) (fromIntegral f) (fromIntegral g) (fromIntegral h)
   in IPv6 w1 w2

-- | Convert an 'IPv6' to eight 16-bit words.
toWord16s :: IPv6 -> (Word16,Word16,Word16,Word16,Word16,Word16,Word16,Word16)
toWord16s (IPv6 a b) =
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
  let !(w1,w2) = fromWord32sV6
        (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)
   in IPv6 w1 w2

-- | Uncurried variant of 'fromWord32s'.
fromTupleWord32s :: (Word32,Word32,Word32,Word32) -> IPv6
fromTupleWord32s (a,b,c,d) = fromWord32s a b c d

-- | Convert an 'IPv6' to four 32-bit words.
toWord32s :: IPv6 -> (Word32,Word32,Word32,Word32)
toWord32s (IPv6 a b) =
  ( fromIntegral (unsafeShiftR a 32)
  , fromIntegral a
  , fromIntegral (unsafeShiftR b 32)
  , fromIntegral b
  )

loopback :: IPv6
loopback = IPv6 0 1

any :: IPv6
any = IPv6 0 0

-- | Encodes the IP, using zero-compression on the leftmost-longest string of
-- zeroes in the address.
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

-- | Decode an IPv6 address. This accepts both standard IPv6
-- notation (with zero compression) and mixed notation for
-- IPv4-mapped IPv6 addresses.
decode :: Text -> Maybe IPv6
decode t = rightToMaybe (AT.parseOnly (parser <* AT.endOfInput) t)

parser :: Atto.Parser IPv6
parser = startIP >>= makeIP
  where
  -- handles the case where an IP starts with ::
  startIP :: Atto.Parser ([Word16], Maybe [Word16])
  startIP = 
    (Atto.char ':' *> Atto.char ':' *> afterDoubleColon []) <|>
    fullIP []

  -- a full IP that might contain double-colon:
  fullIP :: [Word16] -> Atto.Parser ([Word16], Maybe [Word16])
  fullIP starts =
    ((\x -> (x ++ starts, Nothing)) <$> ipv4) <|>
    startPart starts

  -- just after double-colon: either has the rest of the IP or ends
  afterDoubleColon starts = (\ends -> (starts, Just ends)) <$> (restOfIP [] <|> pure [])

  -- the rest of an IP that cannot contain double-colon:
  restOfIP :: [Word16] -> Atto.Parser [Word16]
  restOfIP ends =
    ((\x -> x ++ ends) <$> ipv4) <|>
    endPart ends
  
  ipv4 = ipv4ToWord16s <$> IPv4.parser
  ipv4ToWord16s (IPv4 ip) = [fromIntegral (ip .&. 0xFFFF), fromIntegral (ip `unsafeShiftR` 16)]
  
  -- a colon-separated part before ::
  startPart (starts) = do
    part <- Atto.hexadecimal
    let result = (part : starts)
    Atto.peekChar >>= \case 
      Just ':' -> do
        _ <- Atto.anyChar -- will be ':'
        Atto.peekChar >>= \case
          Just ':' -> do
            _ <- Atto.anyChar -- will be ':' 
            afterDoubleColon result
          _ ->
            fullIP result
      _ ->
        pure (result, Nothing)

  -- a colon-separated part after ::
  endPart ends = do
    part <- Atto.hexadecimal
    let result = part : ends
    Atto.peekChar >>= \case 
      Just ':' -> do
        _ <- Atto.anyChar -- will be ':'
        Atto.peekChar >>= \case
          Just ':' -> do
            fail "Cannot use double colon for omitting zeroes more than once in an IPv6 address"
          _ ->
            restOfIP result
      _ ->
        pure result

  makeIP :: ([Word16], Maybe [Word16]) -> Atto.Parser IPv6
  makeIP (starts, mends) =
    case mends of 
      -- Nothing indicates we never encountered double-colon, so we must have
      -- all 8 parts:
      Nothing -> do
        if numStarts /= 8
        then fail "not enough colon-separated parts in IPv6 address"
        else
          let [w1, w2, w3, w4, w5, w6, w7, w8] = reverse starts in
          pure (fromWord16s w1 w2 w3 w4 w5 w6 w7 w8)
      -- otherwise, we did encounter a double-colon, so we expand it to fill:
      Just ends ->
        let numEnds = length ends in
        case compare (numStarts + numEnds) 8 of
          GT -> fail "too many colon-separated parts in IPv6 address"
          EQ -> fail "unnecessary double-colon in IPv6 address"
          LT -> 
            let [w1, w2, w3, w4, w5, w6, w7, w8] = reverse starts ++ replicate (8 - (numStarts + numEnds)) 0 ++ reverse ends in
            pure (fromWord16s w1 w2 w3 w4 w5 w6 w7 w8)
    where
    numStarts = length starts

fromOctetsV6 ::
     Word64 -> Word64 -> Word64 -> Word64
  -> Word64 -> Word64 -> Word64 -> Word64
  -> Word64 -> Word64 -> Word64 -> Word64
  -> Word64 -> Word64 -> Word64 -> Word64
  -> (Word64,Word64)
fromOctetsV6 a b c d e f g h i j k l m n o p =
  ( fromOctetsWord64 a b c d e f g h
  , fromOctetsWord64 i j k l m n o p
  )

fromWord16sV6 ::
     Word64 -> Word64 -> Word64 -> Word64
  -> Word64 -> Word64 -> Word64 -> Word64
  -> (Word64,Word64)
fromWord16sV6 a b c d e f g h =
  ( fromWord16Word64 a b c d
  , fromWord16Word64 e f g h
  )

fromWord32sV6 :: Word64 -> Word64 -> Word64 -> Word64 -> (Word64,Word64)
fromWord32sV6 a b c d =
  ( fromWord32Word64 a b
  , fromWord32Word64 c d
  )

fromOctetsWord64 ::
     Word64 -> Word64 -> Word64 -> Word64
  -> Word64 -> Word64 -> Word64 -> Word64
  -> Word64
fromOctetsWord64 a b c d e f g h = fromIntegral
    ( shiftL a 56
  .|. shiftL b 48
  .|. shiftL c 40
  .|. shiftL d 32
  .|. shiftL e 24
  .|. shiftL f 16
  .|. shiftL g 8
  .|. h
    )

fromWord16Word64 :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
fromWord16Word64 a b c d = fromIntegral
    ( unsafeShiftL a 48
  .|. unsafeShiftL b 32
  .|. unsafeShiftL c 16
  .|. d
    )

fromWord32Word64 :: Word64 -> Word64 -> Word64
fromWord32Word64 a b = fromIntegral (unsafeShiftL a 32 .|. b)

