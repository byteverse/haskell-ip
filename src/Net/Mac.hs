 {-# LANGUAGE DeriveGeneric #-}
 {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Net.Mac where

import Data.Text (Text)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.Word 
import Data.Aeson (ToJSON(..),FromJSON(..))
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.ByteString.Char8 as AB
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)
import Net.Internal (attoparsecParseJSON)
import qualified Data.Text.Lazy.Builder as TBuilder
import Data.Text.Lazy.Builder.Int (hexadecimal)
import Data.Monoid ((<>))
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy as LText

data Mac = Mac
  { macA :: {-# UNPACK #-} !Word16
  , macB :: {-# UNPACK #-} !Word32
  }
  deriving (Eq,Ord,Show,Read,Generic)

instance Hashable Mac

instance ToJSON Mac where
  toJSON = Aeson.String . toText

instance FromJSON Mac where
  parseJSON = attoparsecParseJSON (textParser <* AT.endOfInput)

toText :: Mac -> Text
toText = LText.toStrict . TBuilder.toLazyText . toTextBuilder

toTextBuilder :: Mac -> TBuilder.Builder
toTextBuilder (Mac a b) = 
  hexadecimal (255 .&. shiftR a 8 )
  <> colon
  <> hexadecimal (255 .&. a )
  <> colon
  <> hexadecimal (255 .&. shiftR b 24 )
  <> colon
  <> hexadecimal (255 .&. shiftR b 16 )
  <> colon
  <> hexadecimal (255 .&. shiftR b 8 )
  <> colon
  <> hexadecimal (255 .&. b)
  where colon = TBuilder.singleton ':'

-- | This does not do an endOfInput check
textParser :: AT.Parser Mac
textParser = fromOctets'
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

bytestringParser :: AB.Parser Mac
bytestringParser = fromOctets'
  <$> (AB.hexadecimal >>= limitSize)
  <*  AB.char ':'
  <*> (AB.hexadecimal >>= limitSize)
  <*  AB.char ':'
  <*> (AB.hexadecimal >>= limitSize)
  <*  AB.char ':'
  <*> (AB.hexadecimal >>= limitSize)
  <*  AB.char ':'
  <*> (AB.hexadecimal >>= limitSize)
  <*  AB.char ':'
  <*> (AB.hexadecimal >>= limitSize)
  where
  limitSize i = 
    if i > 255 
      then fail "All octets in a mac address must be between 00 and FF"
      else return i

fromOctets :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Mac
fromOctets a b c d e f = fromOctets'
  (fromIntegral a) (fromIntegral b) (fromIntegral c)
  (fromIntegral d) (fromIntegral e) (fromIntegral f)

fromOctets' :: Word16 -> Word16 -> Word32 -> Word32 -> Word32 -> Word32 -> Mac
fromOctets' a b c d e f = Mac
    ( shiftL a 8 .|. b )
    ( shiftL c 24 .|. shiftL d 16 .|. shiftL e 8 .|. f )


