{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}


-- |
-- Module      : Data.Word.Word12
-- License     : see  src/Data/LICENSE
-- Stability   : experimental
-- Portability : non-portable (GHC Extensions)

-- Provide a 12-bit unsigned integral type: 'Word12', analagous to Word8,
-- Word16, etc.
--

module Data.Word.Synthetic.Word12 (
  -- * Word12 type
    Word12(..)
  -- * Internal helpers
  , narrow12Word#
  , clz12#
  , ctz12#
  , popCnt12#
  )

where

import           Data.Bits
import           Data.Data
import           Data.Maybe

import           GHC.Arr
import           GHC.Base
import           GHC.Enum
import           GHC.Num
import           GHC.Read
import           GHC.Real
import           GHC.Show
import           GHC.Word

------------------------------------------------------------------------

-- Word12 is represented in the same way as Word.  Operations may assume and
-- must ensure that it holds only values in its logical range.

-- | 12-bit unsigned integer type
--
data Word12 = W12# Word# deriving (Eq, Ord)

word12Type :: DataType
word12Type = mkIntType "Data.Word.Synthetic.Word12.Word12"

instance Data Word12 where
  toConstr x = mkIntegralConstr word12Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word12."
  dataTypeOf _ = word12Type

-- | narrowings represented as primop 'and#' in GHC.
narrow12Word# :: Word# -> Word#
narrow12Word# = and# 0xFFF##

-- | count leading zeros
--
clz12# :: Word# -> Word#
clz12# w# = clz32# (narrow12Word# w#) `minusWord#` 20##

-- | count trailing zeros
--
ctz12# :: Word# -> Word#
ctz12# w# = ctz# w#

-- | the number of set bits
--
popCnt12# :: Word# -> Word#
popCnt12# w# = popCnt# (narrow12Word# w#)

instance Show Word12 where
  showsPrec p x = showsPrec p (fromIntegral x :: Int)

instance Num Word12 where
  (W12# x#) + (W12# y#) = W12# (narrow12Word# (x# `plusWord#` y#))
  (W12# x#) - (W12# y#) = W12# (narrow12Word# (x# `minusWord#` y#))
  (W12# x#) * (W12# y#) = W12# (narrow12Word# (x# `timesWord#` y#))
  negate (W12# x#)      = W12# (narrow12Word# (int2Word# (negateInt# (word2Int# x#))))
  abs x                 = x
  signum 0              = 0
  signum _              = 1
  fromInteger i         = W12# (narrow12Word# (integerToWord i))

instance Real Word12 where
  toRational x = toInteger x % 1

instance Enum Word12 where
  succ x
    | x /= maxBound  = x + 1
    | otherwise      = succError "Word12"
  pred x
    | x /= minBound  = x - 1
    | otherwise      = predError "Word12"
  toEnum i@(I# i#)
    | i >= 0 && i <= fromIntegral (maxBound :: Word12)
                     = W12# (int2Word# i#)
    | otherwise      = toEnumError "Word12" i (minBound::Word12, maxBound::Word12)
  fromEnum (W12# x#) = I# (word2Int# x#)
  enumFrom           = boundedEnumFrom
  enumFromThen       = boundedEnumFromThen

instance Integral Word12 where
  quot (W12# x#) y@(W12# y#)
    | y /= 0                 = W12# (x# `quotWord#` y#)
    | otherwise              = divZeroError
  rem (W12# x#) y@(W12# y#)
    | y /= 0                 = W12# (x# `remWord#` y#)
    | otherwise              = divZeroError
  div (W12# x#) y@(W12# y#)
    | y /= 0                 = W12# (x# `quotWord#` y#)
    | otherwise              = divZeroError
  mod (W12# x#) y@(W12# y#)
    | y /= 0                 = W12# (x# `remWord#` y#)
    | otherwise              = divZeroError
  quotRem (W12# x#) y@(W12# y#)
    | y /= 0                 = (W12# (x# `quotWord#` y#), W12# (x# `remWord#` y#))
    | otherwise              = divZeroError
  divMod (W12# x#) y@(W12# y#)
    | y /= 0                 = (W12# (x# `quotWord#` y#), W12# (x# `remWord#` y#))
    | otherwise              = divZeroError
  toInteger (W12# x#)        = smallInteger (word2Int# x#)

instance Bounded Word12 where
  minBound = 0
  maxBound = 0xFFFFFF

instance Ix Word12 where
  range (m,n)         = [m..n]
  unsafeIndex (m,_) i = fromIntegral (i - m)
  inRange (m,n) i     = m <= i && i <= n

instance Read Word12 where
  readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

instance Bits Word12 where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    (W12# x#) .&.   (W12# y#)  = W12# (x# `and#` y#)
    (W12# x#) .|.   (W12# y#)  = W12# (x# `or#`  y#)
    (W12# x#) `xor` (W12# y#)  = W12# (x# `xor#` y#)
    complement (W12# x#)       = W12# (x# `xor#` mb#) where !(W12# mb#) = maxBound
    (W12# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#)  = W12# (narrow12Word# (x# `shiftL#` i#))
        | otherwise            = W12# (x# `shiftRL#` negateInt# i#)
    (W12# x#) `shiftL` (I# i#)       = W12# (narrow12Word# (x# `shiftL#` i#))
    (W12# x#) `unsafeShiftL` (I# i#) =
        W12# (narrow12Word# (x# `uncheckedShiftL#` i#))
    (W12# x#) `shiftR`       (I# i#) = W12# (x# `shiftRL#` i#)
    (W12# x#) `unsafeShiftR` (I# i#) = W12# (x# `uncheckedShiftRL#` i#)
    (W12# x#) `rotate`       i
        | isTrue# (i'# ==# 0#) = W12# x#
        | otherwise  = W12# (narrow12Word# ((x# `uncheckedShiftL#` i'#) `or#`
                                            (x# `uncheckedShiftRL#` (12# -# i'#))))
      where
        !(I# i'#) = i `mod` 12
    bitSizeMaybe i            = Just (finiteBitSize i)
    bitSize                   = finiteBitSize
    isSigned _                = False
    popCount (W12# x#)        = I# (word2Int# (popCnt12# x#))
    bit                       = bitDefault
    testBit                   = testBitDefault

instance FiniteBits Word12 where
    finiteBitSize _ = 12
    countLeadingZeros  (W12# x#) = I# (word2Int# (clz12# x#))
    countTrailingZeros (W12# x#) = I# (word2Int# (ctz12# x#))

{-# RULES
"fromIntegral/Word8->Word12"    fromIntegral = \(W8# x#) -> W12# x#
"fromIntegral/Word12->Word12"   fromIntegral = id :: Word12 -> Word12
"fromIntegral/Word12->Integer"  fromIntegral = toInteger :: Word12 -> Integer
"fromIntegral/a->Word12"        fromIntegral = \x -> case fromIntegral x of W# x# -> W12# (narrow12Word# x#)
"fromIntegral/Word12->a"        fromIntegral = \(W12# x#) -> fromIntegral (W# x#)
  #-}

{-# RULES
"properFraction/Float->(Word12,Float)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Word12) n, y :: Float) }
"truncate/Float->Word12"
    truncate = (fromIntegral :: Int -> Word12) . (truncate :: Float -> Int)
"floor/Float->Word12"
    floor    = (fromIntegral :: Int -> Word12) . (floor :: Float -> Int)
"ceiling/Float->Word12"
    ceiling  = (fromIntegral :: Int -> Word12) . (ceiling :: Float -> Int)
"round/Float->Word12"
    round    = (fromIntegral :: Int -> Word12) . (round  :: Float -> Int)
  #-}

{-# RULES
"properFraction/Double->(Word12,Double)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Word12) n, y :: Double) }
"truncate/Double->Word12"
    truncate = (fromIntegral :: Int -> Word12) . (truncate :: Double -> Int)
"floor/Double->Word12"
    floor    = (fromIntegral :: Int -> Word12) . (floor :: Double -> Int)
"ceiling/Double->Word12"
    ceiling  = (fromIntegral :: Int -> Word12) . (ceiling :: Double -> Int)
"round/Double->Word12"
    round    = (fromIntegral :: Int -> Word12) . (round  :: Double -> Int)
  #-}


