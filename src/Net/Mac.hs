 {-# LANGUAGE DeriveGeneric #-}
 {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Net.Mac
  ( fromOctets
  , fromOctetsNoCast
  ) where

import Net.Types (Mac(..))
import Data.Text (Text)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.Word
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)
import Net.Internal (attoparsecParseJSON,rightToMaybe)

fromOctets :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Mac
fromOctets a b c d e f = fromOctetsNoCast
  (fromIntegral a) (fromIntegral b) (fromIntegral c)
  (fromIntegral d) (fromIntegral e) (fromIntegral f)

fromOctetsNoCast :: Word16 -> Word16 -> Word32 -> Word32 -> Word32 -> Word32 -> Mac
fromOctetsNoCast a b c d e f = Mac
    ( shiftL a 8 .|. b )
    ( shiftL c 24 .|. shiftL d 16 .|. shiftL e 8 .|. f )
{-# INLINE fromOctetsNoCast #-}


