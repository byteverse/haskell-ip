 {-# LANGUAGE DeriveGeneric #-}
 {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Net.Mac
  ( fromOctets
  ) where

import Net.Types (Mac(..))
import Data.Text (Text)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.Word
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)
import qualified Net.Internal as Internal

fromOctets :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Mac
fromOctets a b c d e f = Mac $ Internal.unsafeWord48FromOctets
  (fromIntegral a) (fromIntegral b) (fromIntegral c)
  (fromIntegral d) (fromIntegral e) (fromIntegral f)
{-# INLINE fromOctets #-}


