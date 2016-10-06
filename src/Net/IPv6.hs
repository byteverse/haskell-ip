{-# LANGUAGE BangPatterns #-}

module Net.IPv6 where

import qualified Net.Internal as Internal
import Data.Bits
import Data.Word
import Net.Types

fromOctets ::
     Word8 -> Word8 -> Word8 -> Word8
  -> Word8 -> Word8 -> Word8 -> Word8
  -> Word8 -> Word8 -> Word8 -> Word8
  -> Word8 -> Word8 -> Word8 -> Word8
  -> IPv6
fromOctets a b c d e f g h i j k l m n o p =
  let !(w1,w2) = Internal.fromOctetsV6
        (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)
        (fromIntegral e) (fromIntegral f) (fromIntegral g) (fromIntegral h)
        (fromIntegral i) (fromIntegral j) (fromIntegral k) (fromIntegral l)
        (fromIntegral m) (fromIntegral n) (fromIntegral o) (fromIntegral p)
   in IPv6 w1 w2

fromWord16s ::
     Word16 -> Word16 -> Word16 -> Word16
  -> Word16 -> Word16 -> Word16 -> Word16
  -> IPv6
fromWord16s a b c d e f g h =
  let !(w1,w2) = Internal.fromWord16sV6
        (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)
        (fromIntegral e) (fromIntegral f) (fromIntegral g) (fromIntegral h)
   in IPv6 w1 w2

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
