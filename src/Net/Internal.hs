module Net.Internal where

import Data.Monoid ((<>))
import Data.Word
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)
import Control.Monad.ST
import Data.Text.Internal (Text(..))
import Data.ByteString (ByteString)
import Data.Text.Lazy.Builder.Int (decimal)
import qualified Data.Text.Lazy         as LText
import qualified Data.Attoparsec.Text   as AT
import qualified Data.Aeson.Types       as Aeson
import qualified Data.Text.Array        as TArray
import qualified Data.ByteString.Char8  as BC8
import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Lazy.Builder.Int as TBuilder

attoparsecParseJSON :: AT.Parser a -> Aeson.Value -> Aeson.Parser a
attoparsecParseJSON p v =
  case v of
    Aeson.String t ->
      case AT.parseOnly p t of
        Left err  -> fail err
        Right res -> return res
    _ -> fail "expected a String"

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

toDotDecimalText :: Word32 -> Text
toDotDecimalText = toTextPreAllocated
{-# INLINE toDotDecimalText #-}

toDotDecimalBuilder :: Word32 -> TBuilder.Builder
toDotDecimalBuilder = TBuilder.fromText . toTextPreAllocated
{-# INLINE toDotDecimalBuilder #-}

rangeToDotDecimalText :: Word32 -> Word8 -> Text
rangeToDotDecimalText addr len =
  LText.toStrict (TBuilder.toLazyText (rangeToDotDecimalBuilder addr len))

rangeToDotDecimalBuilder :: Word32 -> Word8 -> TBuilder.Builder
rangeToDotDecimalBuilder addr len =
  toDotDecimalBuilder addr
  <> TBuilder.singleton '/'
  <> decimal len

-- | I think that this function can be improved. Right now, it
--   always allocates enough space for a fifteen-character text
--   rendering of an IP address. I think that it should be possible
--   to do more of the math upfront and allocate less space.
toTextPreAllocated :: Word32 -> Text
toTextPreAllocated w =
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

i2w :: Integral a => a -> Word16
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

fromDotDecimalText' :: Text -> Either String Word32
fromDotDecimalText' t =
  AT.parseOnly (dotDecimalParser <* AT.endOfInput) t

fromDotDecimalText :: Text -> Maybe Word32
fromDotDecimalText = rightToMaybe . fromDotDecimalText'

rangeFromDotDecimalText' :: (Word32 -> Word8 -> a) -> Text -> Either String a
rangeFromDotDecimalText' f t =
  AT.parseOnly (dotDecimalRangeParser f <* AT.endOfInput) t
{-# INLINE rangeFromDotDecimalText' #-}

rangeFromDotDecimalText :: (Word32 -> Word8 -> a) -> Text -> Maybe a
rangeFromDotDecimalText f = rightToMaybe . rangeFromDotDecimalText' f

dotDecimalRangeParser :: (Word32 -> Word8 -> a) -> AT.Parser a
dotDecimalRangeParser f = f
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
dotDecimalParser :: AT.Parser Word32
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

-- | This is sort of a misnomer. It takes Word32 to make
--   dotDecimalParser probably perform better. This is mostly
--   for internal use.
fromOctets' :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
fromOctets' a b c d =
    ( shiftL a 24
  .|. shiftL b 16
  .|. shiftL c 8
  .|. d
    )

mask :: Word8 -> Word32
mask = complement . shiftR 0xffffffff . fromIntegral

p24 :: Word32
p24 = fromOctets' 10 0 0 0

p20 :: Word32
p20 = fromOctets' 172 16 0 0

p16 :: Word32
p16 = fromOctets' 192 168 0 0

mask8,mask4,mask12,mask20,mask28,mask16,mask10,mask24,mask32,mask15 :: Word32
mask4  = 0xF0000000
mask8  = 0xFF000000
mask10 = 0xFFC00000
mask12 = 0xFFF00000
mask15 = 0xFFFE0000
mask16 = 0xFFFF0000
mask20 = 0xFFFFF000
mask24 = 0xFFFFFF00
mask28 = 0xFFFFFFF0
mask32 = 0xFFFFFFFF

-- r1,r2,r3,r4,r5,r6 :: Word32
-- r1 = fromOctets' 0 0 0 0

macTextParser :: (Word16 -> Word16 -> Word32 -> Word32 -> Word32 -> Word32 -> a) -> AT.Parser a
macTextParser f = f
  <$> (AT.hexadecimal >>= limitSize)
  <*  AT.char ':'
  <*> (AT.hexadecimal >>= limitSize)
  <*  AT.char ':'
  <*> (AT.hexadecimal >>= limitSize)
  <*  AT.char ':'
  <*> (AT.hexadecimal >>= limitSize)
  <*  AT.char ':'
  <*> (AT.hexadecimal >>= limitSize)
  <*  AT.char ':'
  <*> (AT.hexadecimal >>= limitSize)
  where
  limitSize i =
    if i > 255
      then fail "All octets in a mac address must be between 00 and FF"
      else return i

macToText :: Word16 -> Word32 -> Text
macToText a b = LText.toStrict (TBuilder.toLazyText (macToTextBuilder a b))

macToTextBuilder :: Word16 -> Word32 -> TBuilder.Builder
macToTextBuilder a b =
  TBuilder.hexadecimal (255 .&. shiftR a 8 )
  <> colon
  <> TBuilder.hexadecimal (255 .&. a )
  <> colon
  <> TBuilder.hexadecimal (255 .&. shiftR b 24 )
  <> colon
  <> TBuilder.hexadecimal (255 .&. shiftR b 16 )
  <> colon
  <> TBuilder.hexadecimal (255 .&. shiftR b 8 )
  <> colon
  <> TBuilder.hexadecimal (255 .&. b)
  where colon = TBuilder.singleton ':'

macFromText :: (Word16 -> Word16 -> Word32 -> Word32 -> Word32 -> Word32 -> a) -> Text -> Maybe a
macFromText f = rightToMaybe . macFromText' f
{-# INLINE macFromText #-}

macFromText' :: (Word16 -> Word16 -> Word32 -> Word32 -> Word32 -> Word32 -> a) -> Text -> Either String a
macFromText' f = AT.parseOnly (macTextParser f <* AT.endOfInput)
{-# INLINE macFromText' #-}

