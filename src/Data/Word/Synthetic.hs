{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Word.Synthetic where

import Data.Bits
import Data.Word
import Data.Int
import Data.Ratio
import GHC.Arr
import GHC.Enum
import Data.Primitive.Types
import Data.Hashable (Hashable)
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Generic as GVector
import qualified Data.Vector.Unboxed.Mutable as MUVector
import qualified Data.Vector.Generic.Mutable as MGVector

newtype Word48 = W48 Word64
  deriving (Eq,Ord,Hashable)

narrow48Word :: Word64 -> Word64
narrow48Word = (.&.) 0xFFFFFFFFFFFF
{-# INLINE narrow48Word #-}

popCnt48 :: Word64 -> Int
popCnt48 = popCount . narrow48Word
{-# INLINE popCnt48 #-}

word48UpperWord16 :: Word48 -> Word16
word48UpperWord16 (W48 w) = fromIntegral (unsafeShiftR w 32)

word48LowerWord32 :: Word48 -> Word32
word48LowerWord32 (W48 w) = fromIntegral w

word48FromUpperLower :: Word16 -> Word32 -> Word48
word48FromUpperLower a b = 
  W48 (unsafeShiftL (fromIntegral a) 32 .|. fromIntegral b)

instance Show Word48 where
  showsPrec p (W48 x) = showsPrec p x

instance Num Word48 where
  (W48 x) + (W48 y) = W48 (narrow48Word (x + y))
  (W48 x) - (W48 y) = W48 (narrow48Word (x - y))
  (W48 x) * (W48 y) = W48 (narrow48Word (x * y))
  negate (W48 x) = W48 (negate (fromIntegral (negate (fromIntegral x :: Int64))))
  abs x = x
  signum 0 = 0
  signum _ = 1
  fromInteger i = W48 (narrow48Word (fromInteger i))

instance Real Word48 where
  toRational (W48 x) = toInteger x % 1

instance Enum Word48 where
  succ x
    | x /= maxBound = x + 1
    | otherwise = succError "Word48"
  pred x
    | x /= minBound  = x - 1
    | otherwise      = predError "Word48"
  toEnum i
    | i >= 0 && i <= fromIntegral (maxBound :: Word48)
                     = W48 (fromIntegral i)
    | otherwise      = toEnumError "Word48" i (minBound::Word48, maxBound::Word48)
  -- Causes a bounds check to occur twice
  fromEnum (W48 x)
    | x <= fromIntegral (maxBound :: Int) = fromIntegral x
    | otherwise = fromEnumError "Word48" x
  enumFrom           = boundedEnumFrom
  enumFromThen       = boundedEnumFromThen

instance Integral Word48 where
  quot (W48 x) (W48 y) = W48 (x `quot` y)
  rem (W48 x) (W48 y) = W48 (x `rem` y)
  div (W48 x) (W48 y) = W48 (x `quot` y)
  mod (W48 x) (W48 y) = W48 (x `rem` y)
  quotRem (W48 x) (W48 y) = (W48 (x `quot` y), W48 (x `rem` y))
  divMod (W48 x) (W48 y) = (W48 (x `quot` y), W48 (x `rem` y))
  toInteger (W48 x) = toInteger x

instance Bounded Word48 where
  minBound = 0
  maxBound = 0xFFFFFFFFFFFF

instance Ix Word48 where
  range (m,n)         = [m..n]
  unsafeIndex (m,_) i = fromIntegral (i - m)
  inRange (m,n) i     = m <= i && i <= n

instance Read Word48 where
  readsPrec p s = [(fromIntegral (x::Word64), r) | (x, r) <- readsPrec p s]

instance Bits Word48 where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    (W48 x) .&.   (W48 y)  = W48 (x .&. y)
    (W48 x) .|.   (W48 y)  = W48 (x .|.  y)
    (W48 x) `xor` (W48 y)  = W48 (x `xor` y)
    complement (W48 x) = W48 (x `xor` mb) where !(W48 mb) = maxBound
    (W48 x) `shift` i
        | i >= 0 = W48 (narrow48Word (x `shiftL` i))
        | otherwise = W48 (x `shiftR` negate i)
    (W48 x) `shiftL` i = W48 (narrow48Word (x `shiftL` i))
    (W48 x) `unsafeShiftL` i =
        W48 (narrow48Word (x `unsafeShiftL` i))
    (W48 x) `shiftR`       i = W48 (x `shiftR` i)
    (W48 x) `unsafeShiftR` i = W48 (x `unsafeShiftR` i)
    (W48 x) `rotate`       i
        | i' == 0 = W48 x
        | otherwise = W48 (narrow48Word ((x `unsafeShiftL` i') .|.
                                            (x `unsafeShiftR` (48 - i'))))
      where
        !i' = i `mod` 48
    bitSizeMaybe i = Just (finiteBitSize i)
    bitSize = finiteBitSize
    isSigned _ = False
    popCount (W48 x) = popCnt48 x
    bit = bitDefault
    testBit = testBitDefault

instance FiniteBits Word48 where
    finiteBitSize _ = 48
    countLeadingZeros (W48 x) = countLeadingZeros x - 16
    countTrailingZeros (W48 x) = countTrailingZeros x

data instance MUVector.MVector s Word48
  = MV_Word48
      {-# UNPACK #-} !Int 
      !(MUVector.MVector s Word16)
      !(MUVector.MVector s Word32)

data instance UVector.Vector Word48
  = V_Word48
      {-# UNPACK #-} !Int 
      !(UVector.Vector Word16)
      !(UVector.Vector Word32)

instance UVector.Unbox Word48

instance MGVector.MVector MUVector.MVector Word48 where
  {-# INLINE basicLength  #-}
  basicLength (MV_Word48 n_ as bs) = n_
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (MV_Word48 n_ as bs)
      = MV_Word48 m_ 
          (MGVector.basicUnsafeSlice i_ m_ as)
          (MGVector.basicUnsafeSlice i_ m_ bs)
  {-# INLINE basicOverlaps  #-}
  basicOverlaps (MV_Word48 n_1 as1 bs1) (MV_Word48 n_2 as2 bs2)
      = MGVector.basicOverlaps as1 as2
        || MGVector.basicOverlaps bs1 bs2
  {-# INLINE basicUnsafeNew  #-}
  basicUnsafeNew n_
      = do
          as <- MGVector.basicUnsafeNew n_
          bs <- MGVector.basicUnsafeNew n_
          return $ MV_Word48 n_ as bs
  {-# INLINE basicInitialize  #-}
  basicInitialize (MV_Word48 _ as bs)
      = do
          MGVector.basicInitialize as
          MGVector.basicInitialize bs
  {-# INLINE basicUnsafeReplicate  #-}
  basicUnsafeReplicate n_ w
      = do
          let upper = word48UpperWord16 w
              lower = word48LowerWord32 w
          as <- MGVector.basicUnsafeReplicate n_ upper
          bs <- MGVector.basicUnsafeReplicate n_ lower
          return $ MV_Word48 n_ as bs
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeRead (MV_Word48 n_ as bs) i_
      = do
          a <- MGVector.basicUnsafeRead as i_
          b <- MGVector.basicUnsafeRead bs i_
          return (word48FromUpperLower a b)
  {-# INLINE basicUnsafeWrite  #-}
  basicUnsafeWrite (MV_Word48 n_ as bs) i_ w
      = do
          let upper = word48UpperWord16 w
              lower = word48LowerWord32 w
          MGVector.basicUnsafeWrite as i_ upper
          MGVector.basicUnsafeWrite bs i_ lower
  {-# INLINE basicClear  #-}
  basicClear (MV_Word48 n_ as bs)
      = do
          MGVector.basicClear as
          MGVector.basicClear bs
  {-# INLINE basicSet  #-}
  basicSet (MV_Word48 n_ as bs) w
      = do
          let upper = word48UpperWord16 w
              lower = word48LowerWord32 w
          MGVector.basicSet as upper
          MGVector.basicSet bs lower
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_Word48 n_1 as1 bs1) (MV_Word48 n_2 as2 bs2)
      = do
          MGVector.basicUnsafeCopy as1 as2
          MGVector.basicUnsafeCopy bs1 bs2
  {-# INLINE basicUnsafeMove  #-}
  basicUnsafeMove (MV_Word48 n_1 as1 bs1) (MV_Word48 n_2 as2 bs2)
      = do
          MGVector.basicUnsafeMove as1 as2
          MGVector.basicUnsafeMove bs1 bs2
  {-# INLINE basicUnsafeGrow  #-}
  basicUnsafeGrow (MV_Word48 n_ as bs) m_
      = do
          as' <- MGVector.basicUnsafeGrow as m_
          bs' <- MGVector.basicUnsafeGrow bs m_
          return $ MV_Word48 (m_+n_) as' bs'

instance GVector.Vector UVector.Vector Word48 where
  {-# INLINE basicUnsafeFreeze  #-}
  basicUnsafeFreeze (MV_Word48 n_ as bs)
      = do
          as' <- GVector.basicUnsafeFreeze as
          bs' <- GVector.basicUnsafeFreeze bs
          return $ V_Word48 n_ as' bs'
  {-# INLINE basicUnsafeThaw  #-}
  basicUnsafeThaw (V_Word48 n_ as bs)
      = do
          as' <- GVector.basicUnsafeThaw as
          bs' <- GVector.basicUnsafeThaw bs
          return $ MV_Word48 n_ as' bs'
  {-# INLINE basicLength  #-}
  basicLength (V_Word48 n_ as bs) = n_
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (V_Word48 n_ as bs)
      = V_Word48 m_ (GVector.basicUnsafeSlice i_ m_ as)
                 (GVector.basicUnsafeSlice i_ m_ bs)
  {-# INLINE basicUnsafeIndexM  #-}
  basicUnsafeIndexM (V_Word48 n_ as bs) i_
      = do
          a <- GVector.basicUnsafeIndexM as i_
          b <- GVector.basicUnsafeIndexM bs i_
          return (word48FromUpperLower a b)
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_Word48 n_1 as1 bs1) (V_Word48 n_2 as2 bs2)
      = do
          GVector.basicUnsafeCopy as1 as2
          GVector.basicUnsafeCopy bs1 bs2
  {-# INLINE elemseq  #-}
  elemseq _ = seq

