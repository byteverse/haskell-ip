 {-# LANGUAGE DeriveGeneric #-}
 {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Net.Mac
  ( fromOctets
  , toOctets
  ) where

import Net.Types (Mac(..))
import Data.Word
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement,unsafeShiftR)
import qualified Net.Internal as Internal

fromOctets :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Mac
fromOctets a b c d e f = Mac $ Internal.unsafeWord48FromOctets
  (fromIntegral a) (fromIntegral b) (fromIntegral c)
  (fromIntegral d) (fromIntegral e) (fromIntegral f)
{-# INLINE fromOctets #-}

toOctets :: Mac -> (Word8,Word8,Word8,Word8,Word8,Word8)
toOctets (Mac w) =
  ( fromIntegral $ unsafeShiftR w 40
  , fromIntegral $ unsafeShiftR w 32
  , fromIntegral $ unsafeShiftR w 24
  , fromIntegral $ unsafeShiftR w 16
  , fromIntegral $ unsafeShiftR w 8
  , fromIntegral w
  )

