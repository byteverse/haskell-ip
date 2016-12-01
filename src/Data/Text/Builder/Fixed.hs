{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -O2 -Wall -funbox-strict-fields #-}

{-| For concatenating fixed-width strings that are only a few
    characters each, this can be ten times faster than the builder
    that ships with @text@.
-}
module Data.Text.Builder.Fixed
  ( Builder
  , fromText
  , run
  , contramapBuilder
  , char
  , word8HexFixedLower
  , word8HexFixedUpper
  , word12HexFixedLower
  , word12HexFixedUpper
  ) where

import Data.Text.Internal (Text(..))
import Control.Monad.ST
import Data.Monoid
import Data.Word
import Data.Bits
import Text.Printf
import Debug.Trace
import Data.Char (ord)
import Data.Word.Synthetic (Word12)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Lazy.Builder.Int as TBuilder
import qualified Data.Text.IO as Text
import qualified Data.Text.Array as A

data Builder a where
  BuilderStatic :: Text -> Builder a
#ifdef ghcjs_HOST_OS
  BuilderFunction :: (a -> DListText) -> Builder a
#else
  BuilderFunction :: Text -> (forall s. Int -> A.MArray s -> a -> ST s ()) -> Builder a
#endif

#ifdef ghcjs_HOST_OS
newtype DListText = DListText ([Text] -> [Text])

dlistCons :: Text -> DListText -> DListText
dlistCons a (DListText f) = DListText ((a:) . f)

dlistSnoc :: DListText -> Text -> DListText
dlistSnoc (DListText f) a = DListText (f . (a:))

dlistSingleton :: Text -> DListText
dlistSingleton t = DListText (t:)

dlistToText :: DListText -> Text
dlistToText (DListText f) = Text.concat (f [])

instance Monoid DListText where
  mempty = DListText id
  mappend (DListText f) (DListText g) = DListText (f . g)
#endif

instance Monoid (Builder a) where
  {-# INLINE mempty #-}
  mempty = BuilderStatic Text.empty
  {-# INLINE mappend #-}
#ifdef ghcjs_HOST_OS
  mappend x y = case x of
    BuilderStatic t1 -> case y of
      BuilderStatic t2 -> BuilderStatic (t1 <> t2)
      BuilderFunction f -> BuilderFunction (\a -> dlistCons t1 (f a))
    BuilderFunction f1 -> case y of
      BuilderStatic t2 -> BuilderFunction (\a -> dlistSnoc (f1 a) t2)
      BuilderFunction f2 -> BuilderFunction (\a -> f1 a <> f2 a)
#else
  mappend x y = case x of
    BuilderStatic t1@(Text _ _ len1) -> case y of
      BuilderStatic t2 -> BuilderStatic (t1 <> t2)
      BuilderFunction t2 f -> BuilderFunction (t1 <> t2) (\ix marr a -> f (ix + len1) marr a)
    BuilderFunction t1@(Text _ _ len1) f1 -> case y of
      BuilderStatic t2 -> BuilderFunction (t1 <> t2) f1
      BuilderFunction t2 f2 -> BuilderFunction (t1 <> t2) (\ix marr a -> f1 ix marr a >> f2 (ix + len1) marr a)
#endif

fromText :: Text -> Builder a
fromText = BuilderStatic
{-# INLINE fromText #-}

contramapBuilder :: (b -> a) -> Builder a -> Builder b
contramapBuilder f x = case x of
  BuilderStatic t -> BuilderStatic t
#ifdef ghcjs_HOST_OS
  BuilderFunction g -> BuilderFunction (\b -> g (f b))
#else
  BuilderFunction t g -> BuilderFunction t (\ix marr b -> g ix marr (f b))
#endif
{-# INLINE contramapBuilder #-}

run :: Builder a -> a -> Text
run x a = case x of
  BuilderStatic t -> t
#ifdef ghcjs_HOST_OS
  BuilderFunction f -> dlistToText (f a)
#else
  BuilderFunction (Text inArr off len) f ->
    let outArr = runST $ do
          marr <- A.new len
          A.copyI marr 0 inArr off len
          f 0 marr a
          A.unsafeFreeze marr
     in Text outArr 0 len
#endif

word8HexFixedUpper :: Builder Word8
word8HexFixedUpper = word8HexFixedGeneral True
{-# INLINE word8HexFixedUpper #-}

word8HexFixedLower :: Builder Word8
word8HexFixedLower = word8HexFixedGeneral False
{-# INLINE word8HexFixedLower #-}

-- The Bool is True if the hex digits are upper case.
word8HexFixedGeneral :: Bool -> Builder Word8
word8HexFixedGeneral upper =
#ifdef ghcjs_HOST_OS
  BuilderFunction $ \w ->
    let !ts = if upper then hexValuesWord8UpperTexts else hexValuesWord8LowerTexts
     in dlistSingleton (Vector.unsafeIndex ts (fromIntegral w))
#else
  BuilderFunction (Text.pack "--") $ \i marr w -> do
    let ix = unsafeShiftL (fromIntegral w) 1
        ix2 = ix + 1
        arr = if upper then hexValuesWord8Upper else hexValuesWord8Lower
    A.unsafeWrite marr i (A.unsafeIndex arr ix)
    A.unsafeWrite marr (i + 1) (A.unsafeIndex arr ix2)
#endif
{-# INLINE word8HexFixedGeneral #-}

-- | Characters outside the basic multilingual plane are not handled
--   correctly by this function. They will not cause a program to crash;
--   instead, the character will have the upper bits masked out.
char :: Builder Char
char =
#ifdef ghcjs_HOST_OS
  BuilderFunction (dlistSingleton . Text.singleton)
#else
  BuilderFunction (Text.pack "-") $ \i marr c -> A.unsafeWrite marr i (fromIntegral (ord c))
#endif
{-# INLINE char #-}

word12HexFixedGeneral :: Bool -> Builder Word12
word12HexFixedGeneral upper =
#ifdef ghcjs_HOST_OS
  BuilderFunction $ \w ->
    let !ts = if upper then hexValuesWord12UpperTexts else hexValuesWord12LowerTexts
     in dlistSingleton (Vector.unsafeIndex ts (fromIntegral w))
#else
  BuilderFunction (Text.pack "---") $ \i marr w -> do
    let !wInt = fromIntegral w
        !ix = wInt + wInt + wInt
        !arr = if upper then hexValuesWord12Upper else hexValuesWord12Lower
    A.unsafeWrite marr i (A.unsafeIndex arr ix)
    A.unsafeWrite marr (i + 1) (A.unsafeIndex arr (ix + 1))
    A.unsafeWrite marr (i + 2) (A.unsafeIndex arr (ix + 2))
#endif
{-# INLINE word12HexFixedGeneral #-}

word12HexFixedUpper :: Builder Word12
word12HexFixedUpper = word12HexFixedGeneral True
{-# INLINE word12HexFixedUpper #-}

word12HexFixedLower :: Builder Word12
word12HexFixedLower = word12HexFixedGeneral False
{-# INLINE word12HexFixedLower #-}

hexValuesWord12UpperTexts :: Vector Text
hexValuesWord12UpperTexts = Vector.fromList
  (map (Text.pack . printf "%03X") [0 :: Int ..4096])
{-# NOINLINE hexValuesWord12UpperTexts #-}

hexValuesWord12LowerTexts :: Vector Text
hexValuesWord12LowerTexts = Vector.fromList
  (map (Text.pack . printf "%03x") [0 :: Int ..4096])
{-# NOINLINE hexValuesWord12LowerTexts #-}

hexValuesWord8UpperTexts :: Vector Text
hexValuesWord8UpperTexts = Vector.fromList
  (map (Text.pack . printf "%02X") [0 :: Int ..255])
{-# NOINLINE hexValuesWord8UpperTexts #-}

hexValuesWord8LowerTexts :: Vector Text
hexValuesWord8LowerTexts = Vector.fromList
  (map (Text.pack . printf "%02x") [0 :: Int ..255])
{-# NOINLINE hexValuesWord8LowerTexts #-}

#ifdef ghcjs_HOST_OS
#else
hexValuesWord12Upper :: A.Array
hexValuesWord12Upper = let Text arr _ _ = Text.copy $ fold hexValuesWord12UpperTexts
{-# NOINLINE hexValuesWord12Upper #-}

hexValuesWord12Lower :: A.Array
hexValuesWord12Lower = let Text arr _ _ = Text.copy $ fold hexValuesWord12LowerTexts
{-# NOINLINE hexValuesWord12Lower #-}

hexValuesWord8Upper :: A.Array
hexValuesWord8Upper = let Text arr _ _ = Text.copy $ fold hexValuesWord8UpperTexts
{-# NOINLINE hexValuesWord8Upper #-}

hexValuesWord8Lower :: A.Array
hexValuesWord8Lower = let Text arr _ _ = Text.copy $ fold hexValuesWord8LowerTexts
{-# NOINLINE hexValuesWord8Lower #-}
#endif

