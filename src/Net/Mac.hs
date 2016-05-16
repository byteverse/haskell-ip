 {-# LANGUAGE DeriveGeneric #-}
 {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Net.Mac where

import Data.Text (Text)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.Word 
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.ByteString.Char8 as AB
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)

data Mac = Mac
  { macA :: Word16
  , macB :: Word32
  }
  deriving (Eq,Ord,Show,Read,Generic)

instance Hashable Mac

-- This does not do an endOfInput check because it is
-- reused in the range parser implementation.
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


