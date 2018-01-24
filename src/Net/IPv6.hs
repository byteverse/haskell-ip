{-# LANGUAGE BangPatterns #-}

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
  ) where

import Prelude hiding (any)
import Data.Bits
import Data.List (intercalate, group)
import Data.Word
import Data.Char (chr)
import Control.Applicative
import Data.Text (Text)
import Text.Read (Read(..),Lexeme(Ident),lexP,parens)
import Text.ParserCombinators.ReadPrec (prec,step)
import qualified Data.Text as Text
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as AT
import Numeric (showHex)

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
encode :: IPv6 -> Text
encode ip = toText [w1, w2, w3, w4, w5, w6, w7, w8]
  where
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

decode :: Text -> Maybe IPv6
decode t = rightToMaybe (AT.parseOnly (parser <* AT.endOfInput) t)

parser :: Atto.Parser IPv6
parser = do
  s <- start
  case toIPv6 s of
    Nothing -> fail "Wrong number of octets in IPv6 address"
    Just ip -> return ip
  where
  msg = "All chunks in a formatted IPv6 address must be between 0x0000 and 0xFFFF"
  colonMsg = "Cannot use double colon for omitting zeroes more than once in an IPv6 address"
  start = do
    c <- Atto.peekChar'
    if c == ':'
      then go (-1) 0 []
      else Atto.hexadecimal >>= \w -> go (-1) 1 [w]
    -- r <- fmap Just Atto.hexadecimal <|> (Nothing <$ Atto.char ':')
    -- case r of
    --   Just !w -> go (-1) 1 [w]
    --   Nothing -> go 0 0 []
  go !colonIndex !currentIndex !ws = do
    r <- do
      m <- Atto.peekChar
      case m of
        Nothing -> return ResDone
        Just c -> if c == ':'
          then do
            _ <- Atto.anyChar -- should be a colon
            if colonIndex == currentIndex
              then fmap ResWord Atto.hexadecimal <|> pure ResDone
              else do
                d <- Atto.peekChar'
                if d == ':'
                  then return ResColon
                  else fmap ResWord Atto.hexadecimal
          else return ResDone
    case r of
      ResDone -> pure (S colonIndex currentIndex ws)
      ResColon -> if alreadySet colonIndex
        then fail colonMsg
        else go currentIndex currentIndex ws
      ResWord w -> restrictTo16 msg w >> go colonIndex (currentIndex + 1) (w : ws)

toIPv6 :: S -> Maybe IPv6
toIPv6 (S colonIndex total input) = case compare total 8 of
  EQ -> if colonIndex == (-1)
    then go 0 0 input
    else Nothing
  GT -> Nothing
  LT -> go 0 0 input
  where
  revColonIndex = total - colonIndex
  spacesToSkip = 8 - total
  go :: Int -> Word64 -> [Word64] -> Maybe IPv6
  go !ix !acc ws = if ix > 3
    then fmap (flip IPv6 acc) (go2 ix 0 ws)
    else case ws of
      w : wsNext -> if ix == revColonIndex
        then go (ix + spacesToSkip) acc (w : wsNext)
        else go (ix + 1) (acc .|. unsafeShiftL w (times16 ix)) wsNext
      [] -> if ix == revColonIndex
        then Just $ IPv6 0 acc
        else Nothing -- Not enough word16s in list
  go2 :: Int -> Word64 -> [Word64] -> Maybe Word64
  go2 !ix !acc ws = case ws of
    w : wsNext -> if ix == revColonIndex
      then go2 (ix + spacesToSkip) acc (w : wsNext)
      else go2 (ix + 1) (acc .|. unsafeShiftL w (times16 ix - 64)) wsNext
    [] -> if ix == revColonIndex || ix > 7
      then Just acc
      else Nothing -- Not enough word16s in list

times16 :: Int -> Int
times16 a = unsafeShiftL a 4

alreadySet :: Int -> Bool
alreadySet i = i /= (-1)

restrictTo16 :: String -> Word64 -> Atto.Parser ()
restrictTo16 msg w = if w > 65535
  then fail msg
  else return ()

-- | This is an internal data type used as the result
--   after parsing an ipv6 address. The first field
--   indicates the index at which a double colon occurs.
--   The second is the length of the third.
--   The third is a reversed list of the 16s
--   that comprise the ipv6 address.
data S = S
  { _sDoubleColon :: {-# UNPACK #-} !Int
  , _sTotal :: {-# UNPACK #-} !Int
  , _sRevWords :: ![Word64]
  } deriving (Show,Read)

data Res
  = ResWord {-# UNPACK #-} !Word64
  | ResColon
  | ResDone

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

