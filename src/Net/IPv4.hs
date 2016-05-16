 {-# LANGUAGE DeriveGeneric #-}
 {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Net.IPv4 where

import Data.Text (Text)
import qualified Data.Text.Lazy         as LText
import qualified Data.Text.Lazy.Builder as TBuilder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Monoid ((<>))
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)
import Data.Word
import Data.Hashable
import Data.Aeson (FromJSON(..),ToJSON(..))
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.Text as Attoparsec

newtype IPv4 = IPv4 { getIPv4 :: Word32 }
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Hashable,Generic)

data IPv4Range = IPv4Range 
  { ipv4RangeBase   :: {-# UNPACK #-} !IPv4
  , ipv4RangeLength :: {-# UNPACK #-} !Int
  } deriving (Eq,Ord,Show,Read,Generic)

instance Hashable IPv4Range

instance ToJSON IPv4 where
  toJSON addr = Aeson.String (toDotDecimalText addr)

instance FromJSON IPv4 where
  parseJSON (Aeson.String t) = 
    case fromDotDecimalText' t of
      Left err  -> fail err
      Right res -> return res

instance ToJSON IPv4Range where
  toJSON addrRange = Aeson.String (rangeToDotDecimalText addrRange)

instance FromJSON IPv4Range where
  parseJSON (Aeson.String t) = 
    case rangeFromDotDecimalText' t of
      Left err  -> fail err
      Right res -> return res

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

-- mask :: Int -> IPv4
-- mask w = IPv4 $ complement $ 0xffffffff `shiftR` w

fromDotDecimalText' :: Text -> Either String IPv4
fromDotDecimalText' t = 
  Attoparsec.parseOnly (dotDecimalParser <* Attoparsec.endOfInput) t

fromDotDecimalText :: Text -> Maybe IPv4
fromDotDecimalText = rightToMaybe . fromDotDecimalText'

rangeFromDotDecimalText' :: Text -> Either String IPv4Range
rangeFromDotDecimalText' t = 
  Attoparsec.parseOnly (dotDecimalRangeParser <* Attoparsec.endOfInput) t

rangeFromDotDecimalText :: Text -> Maybe IPv4Range
rangeFromDotDecimalText = rightToMaybe . rangeFromDotDecimalText'

dotDecimalRangeParser :: Attoparsec.Parser IPv4Range
dotDecimalRangeParser = IPv4Range
  <$> dotDecimalParser
  <*  Attoparsec.char '/'
  <*> (Attoparsec.decimal >>= limitSize)
  where
  limitSize i = 
    if i > 32
      then fail "An IP range length must be between 0 and 32"
      else return i

-- This does not do an endOfInput check because it is
-- reused in the range parser implementation.
dotDecimalParser :: Attoparsec.Parser IPv4
dotDecimalParser = fromOctets'
  <$> (Attoparsec.decimal >>= limitSize)
  <*  Attoparsec.char '.'
  <*> (Attoparsec.decimal >>= limitSize)
  <*  Attoparsec.char '.'
  <*> (Attoparsec.decimal >>= limitSize)
  <*  Attoparsec.char '.'
  <*> (Attoparsec.decimal >>= limitSize)
  where
  limitSize i = 
    if i > 255 
      then fail "All octets in an ip address must be between 0 and 255"
      else return i

fromOctets :: Word8 -> Word8 -> Word8 -> Word8 -> IPv4
fromOctets a b c d = fromOctets' 
  (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)

-- | This is sort of a misnomer. It takes Word32 to make
--   dotDecimalParser probably perform better.
fromOctets' :: Word32 -> Word32 -> Word32 -> Word32 -> IPv4
fromOctets' a b c d = IPv4 
    ( 0
  .|. shiftL a 24
  .|. shiftL b 16
  .|. shiftL c 8
  .|. d
    )

toDotDecimalText :: IPv4 -> Text
toDotDecimalText = LText.toStrict . TBuilder.toLazyText . toDotDecimalBuilder

-- It should be possible to write a more efficient version that initially
-- allocates a block of strict text of length 15 and then starts filling 
-- it in.
toDotDecimalBuilder :: IPv4 -> TBuilder.Builder
toDotDecimalBuilder (IPv4 w) = 
  decimal (255 .&. shiftR w 24 )
  <> dot
  <> decimal (255 .&. shiftR w 16 )
  <> dot
  <> decimal (255 .&. shiftR w 8 )
  <> dot
  <> decimal (255 .&. w)
  where dot = TBuilder.singleton '.'

rangeToDotDecimalText :: IPv4Range -> Text
rangeToDotDecimalText = LText.toStrict . TBuilder.toLazyText . rangeToDotDecimalBuilder

rangeToDotDecimalBuilder :: IPv4Range -> TBuilder.Builder
rangeToDotDecimalBuilder (IPv4Range addr len) = 
  toDotDecimalBuilder addr
  <> TBuilder.singleton '/'
  <> decimal len



