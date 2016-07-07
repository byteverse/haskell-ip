module Net.Internal where

import Data.Monoid ((<>))
import Data.Word
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)
import Control.Monad.ST
import Data.Text.Internal (Text(..))
import Data.ByteString (ByteString)
import Data.Text.Lazy.Builder.Int (decimal)
import Control.Monad
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as LText
import qualified Data.Attoparsec.Text   as AT
import qualified Data.Aeson.Types       as Aeson
import qualified Data.Text.Array        as TArray
import qualified Data.ByteString.Char8  as BC8
import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Read         as TextRead
import qualified Data.Text.Lazy.Builder.Int as TBuilder

eitherToAesonParser :: Either String a -> Aeson.Parser a
eitherToAesonParser x = case x of
  Left err -> fail err
  Right a -> return a

attoparsecParseJSON :: AT.Parser a -> Aeson.Value -> Aeson.Parser a
attoparsecParseJSON p v =
  case v of
    Aeson.String t ->
      case AT.parseOnly p t of
        Left err  -> fail err
        Right res -> return res
    _ -> fail "expected a String"

stripDecimal :: Text -> Either String Text
stripDecimal t = case Text.uncons t of
  Nothing -> Left "expected a dot but input ended instead"
  Just (c,tnext) -> if c == '.'
    then Right tnext
    else Left "expected a dot but found a different character"
{-# INLINE stripDecimal #-}

decodeIPv4TextReader :: TextRead.Reader Word32
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
{-# INLINE decodeIPv4TextReader #-}

decodeIPv4TextEither :: Text -> Either String Word32
decodeIPv4TextEither t = case decodeIPv4TextReader t of
  Left err -> Left err
  Right (w,t') -> if Text.null t'
    then Right w
    else Left "expected end of text but it continued instead"

ipOctetSizeErrorMsg :: String
ipOctetSizeErrorMsg = "All octets in an IPv4 address must be between 0 and 255"

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

putMac :: ByteString -> Int -> Int -> TArray.MArray s -> ST s ()
putMac hexPairs pos w marr = do
  let i = w + w
  TArray.unsafeWrite marr pos $ fromIntegral $ ByteString.unsafeIndex hexPairs i
  TArray.unsafeWrite marr (pos + 1) $ fromIntegral $ ByteString.unsafeIndex hexPairs (i + 1)
{-# INLINE putMac #-}

macToTextPreAllocated :: Word8 -> Bool -> Word16 -> Word32 -> Text
macToTextPreAllocated separator' isUpperCase wa wb =
  let w1 = fromIntegral $ 255 .&. shiftR wa 8
      w2 = fromIntegral $ 255 .&. wa
      w3 = fromIntegral $ 255 .&. shiftR wb 24
      w4 = fromIntegral $ 255 .&. shiftR wb 16
      w5 = fromIntegral $ 255 .&. shiftR wb 8
      w6 = fromIntegral $ 255 .&. wb
      hexPairs = if isUpperCase then twoHexDigits else twoHexDigitsLower
      separator = fromIntegral separator' :: Word16
      arr = runST $ do
        marr <- TArray.new 17
        putMac hexPairs 0 w1 marr
        TArray.unsafeWrite marr 2 separator
        putMac hexPairs 3 w2 marr
        TArray.unsafeWrite marr 5 separator
        putMac hexPairs 6 w3 marr
        TArray.unsafeWrite marr 8 separator
        putMac hexPairs 9 w4 marr
        TArray.unsafeWrite marr 11 separator
        putMac hexPairs 12 w5 marr
        TArray.unsafeWrite marr 14 separator
        putMac hexPairs 15 w6 marr
        TArray.unsafeFreeze marr
  in Text arr 0 17
{-# INLINE macToTextPreAllocated #-}

zero :: Word16
zero = 48
{-# INLINE zero #-}

i2w :: Integral a => a -> Word16
i2w v = zero + fromIntegral v
{-# INLINE i2w #-}


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
      then fail ipOctetSizeErrorMsg
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

twoHexDigits :: ByteString
twoHexDigits = BC8.pack
  "000102030405060708090A0B0C0D0E0F\
  \101112131415161718191A1B1C1D1E1F\
  \202122232425262728292A2B2C2D2E2F\
  \303132333435363738393A3B3C3D3E3F\
  \404142434445464748494A4B4C4D4E4F\
  \505152535455565758595A5B5C5D5E5F\
  \606162636465666768696A6B6C6D6E6F\
  \707172737475767778797A7B7C7D7E7F\
  \808182838485868788898A8B8C8D8E8F\
  \909192939495969798999A9B9C9D9E9F\
  \A0A1A2A3A4A5A6A7A8A9AAABACADAEAF\
  \B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF\
  \C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF\
  \D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF\
  \E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF\
  \F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF"

twoHexDigitsLower :: ByteString
twoHexDigitsLower = BC8.pack
  "000102030405060708090a0b0c0d0e0f\
  \101112131415161718191a1b1c1d1e1f\
  \202122232425262728292a2b2c2d2e2f\
  \303132333435363738393a3b3c3d3e3f\
  \404142434445464748494a4b4c4d4e4f\
  \505152535455565758595a5b5c5d5e5f\
  \606162636465666768696a6b6c6d6e6f\
  \707172737475767778797a7b7c7d7e7f\
  \808182838485868788898a8b8c8d8e8f\
  \909192939495969798999a9b9c9d9e9f\
  \a0a1a2a3a4a5a6a7a8a9aaabacadaeaf\
  \b0b1b2b3b4b5b6b7b8b9babbbcbdbebf\
  \c0c1c2c3c4c5c6c7c8c9cacbcccdcecf\
  \d0d1d2d3d4d5d6d7d8d9dadbdcdddedf\
  \e0e1e2e3e4e5e6e7e8e9eaebecedeeef\
  \f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"
