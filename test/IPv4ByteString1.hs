module IPv4ByteString1 where

import Net.Types (IPv4(..))
import Net.IPv4

import Data.ByteString.Internal as I
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import Data.Word

encode :: IPv4 -> ByteString
encode (IPv4 w) = I.unsafeCreateUptoN 15 (\ptr1 ->
  do (len1,ptr2) <- writeWord ptr1 w1
     poke ptr2 dot
     (len2,ptr3) <- writeWord (ptr2 `plusPtr` 1) w2
     poke ptr3 dot
     (len3,ptr4) <- writeWord (ptr3 `plusPtr` 1) w3
     poke ptr4 dot
     (len4,_) <- writeWord (ptr4 `plusPtr` 1) w4
     return (3 + len1 + len2 + len3 + len4))
  where w1 = fromIntegral $ shiftR w 24
        w2 = fromIntegral $ shiftR w 16
        w3 = fromIntegral $ shiftR w 8
        w4 = fromIntegral w
        dot = 46
        writeWord :: Ptr Word8 -> Word8 -> IO (Int,Ptr Word8)
        writeWord ptr word
          | word >= 100 = do
              let (word1,char3) = word `quotRem` 10
                  (char1,char2) = word1 `quotRem` 10
              poke ptr (char1 + 48)
              poke (ptr `plusPtr` 1) (char2 + 48)
              poke (ptr `plusPtr` 2) (char3 + 48)
              return (3,ptr `plusPtr` 3)
          | word >= 10 = do
              let (char1,char2) = word `quotRem` 10
              poke ptr (char1 + 48)
              poke (ptr `plusPtr` 1) (char2 + 48)
              return (2,ptr `plusPtr` 2)
          | otherwise = do
              poke ptr (word + 48)
              return (1,ptr `plusPtr` 1)

