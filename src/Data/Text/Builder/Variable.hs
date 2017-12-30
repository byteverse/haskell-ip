{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

{-| This is a builder optimized for concatenating short
    variable-length strings whose length has a known upper
    bound. In these cases, this can be up to ten times faster
    than the builder provided by the @text@ library. However,
    data whose textual encoding has no known upper bound cannot
    be encoded by the builder provided here. For example, it
    is possible to provide decimal builders for types like 'Int8' and
    'Word16', whose lengths are respectively bounded by
    4 and 5. However, this is not possible for 'Integer', since
    its decimal representation could be arbitrarily long.
-}
module Data.Text.Builder.Variable
  ( Builder
  , run
  , contramap
  , charBmp
  , staticCharBmp
  , word8
  ) where

import Data.Word
import Data.Text (Text)
import Control.Monad.ST
import Data.Char (ord)
import Data.Vector (Vector)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vector
import qualified Data.Text.Array as A
import qualified Data.Text.Builder.Common.Internal as I
import qualified Data.Text.Internal as TI

data Builder a
  = Builder
      {-# UNPACK #-} !Int -- the maximum length, not a character count
      !(forall s. Int -> A.MArray s -> a -> ST s Int)

instance Monoid (Builder a) where
  {-# INLINE mempty #-}
  mempty = Builder 0 (\i _ _ -> return i)
  {-# INLINE mappend #-}
  mappend (Builder len1 f) (Builder len2 g) =
    Builder (len1 + len2) $ \ix1 marr a -> do
      ix2 <- f ix1 marr a
      g ix2 marr a

run :: Builder a -> a -> Text
run (Builder maxLen f) = \a ->
  let (outArr,len) = A.run2 $ do
        marr <- A.new maxLen
        finalIx <- f 0 marr a
        return (marr,finalIx)
   in TI.text outArr 0 len
{-# INLINE run #-}

contramap :: (b -> a) -> Builder a -> Builder b
contramap f (Builder len g) = Builder len $ \i marr b ->
  g i marr (f b)
{-# INLINE contramap #-}

-- finish writing this. it's important for completeness
-- char :: Builder a
-- char = Builder 2 $ \

-- | A character in the basic multilingual plane.
charBmp :: Builder Char
charBmp = Builder 1 $ \i marr c -> do
  A.unsafeWrite marr i (fromIntegral (ord c))
  return (i + 1)
{-# INLINE charBmp #-}

staticCharBmp :: Char -> Builder a
staticCharBmp c = Builder 1 $ \i marr _ -> do
  A.unsafeWrite marr i (fromIntegral (ord c))
  return (i + 1)
{-# INLINE staticCharBmp #-}

word8 :: Builder Word8
word8 = Builder 3 $ \pos marr w -> if
  | w < 10 -> do
      A.unsafeWrite marr pos (i2w w)
      return (pos + 1)
  | w < 100 -> do
      let wInt = fromIntegral w
          ix = wInt + wInt
      A.unsafeWrite marr pos (A.unsafeIndex I.twoDecimalDigits ix)
      A.unsafeWrite marr (pos + 1) (A.unsafeIndex I.twoDecimalDigits (ix + 1))
      return (pos + 2)
  | otherwise -> do
      let wInt = fromIntegral w
          ix = wInt + wInt + wInt
      A.unsafeWrite marr pos (A.unsafeIndex I.threeDecimalDigits ix)
      A.unsafeWrite marr (pos + 1) (A.unsafeIndex I.threeDecimalDigits (ix + 1))
      A.unsafeWrite marr (pos + 2) (A.unsafeIndex I.threeDecimalDigits (ix + 2))
      return (pos + 3)
{-# INLINE word8 #-}

-- This has not yet been tested.
vector ::
     Text -- ^ Default, used when index is out of range
  -> Vector Text -- ^ Texts to index into
  -> Builder Int
vector tDef v =
  let xs = Vector.map I.portableUntext v
      xDef = I.portableUntext tDef
   in Builder
        (Vector.maximum $ Vector.map I.portableTextLength $ Vector.cons tDef v)
        $ \_ marr i -> do
          let (arr,len) = fromMaybe xDef (xs Vector.!? i)
              finalIx = i + len
          A.copyI marr i arr 0 finalIx
          return finalIx
{-# INLINE vector #-}

i2w :: Integral a => a -> Word16
i2w v = asciiZero + fromIntegral v
{-# INLINE i2w #-}

asciiZero :: Word16
asciiZero = 48
{-# INLINE asciiZero #-}

