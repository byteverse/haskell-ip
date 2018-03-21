{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wall -funbox-strict-fields #-}

{-| For concatenating fixed-width strings that are only a few
    characters each, this can be ten times faster than the builder
    that ships with @text@.
-}
module Data.Text.Builder.Fixed
  ( Builder
  , fromText
  , run
  , contramapBuilder
  , charBmp
  , word8HexFixedLower
  , word8HexFixedUpper
  , word12HexFixedLower
  , word12HexFixedUpper
  ) where

import Control.Monad.ST
import Data.Monoid
import Data.Word
import Data.Bits
import Data.Char (ord)
import Data.Word.Synthetic.Word12 (Word12)
import Data.Text (Text)
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import qualified Data.Text.Builder.Common.Internal as I

data Builder a where
  BuilderStatic :: Text -> Builder a
  BuilderFunction :: Text -> (forall s. Int -> A.MArray s -> a -> ST s ()) -> Builder a

{-# INLINE appendBuilder #-}
appendBuilder :: Builder a -> Builder a -> Builder a
appendBuilder x y = case x of
  BuilderStatic t1 -> case y of
    BuilderStatic t2 -> BuilderStatic (t1 <> t2)
    BuilderFunction t2 f ->
      let len1 = I.portableTextLength t1
       in BuilderFunction (t1 <> t2) (\ix marr a -> f (ix + len1) marr a)
  BuilderFunction t1 f1 -> case y of
    BuilderStatic t2 -> BuilderFunction (t1 <> t2) f1
    BuilderFunction t2 f2 ->
      let len1 = I.portableTextLength t1
       in BuilderFunction (t1 <> t2) (\ix marr a -> f1 ix marr a >> f2 (ix + len1) marr a)

instance Semigroup.Semigroup (Builder a) where
  {-# INLINE (<>) #-}
  (<>) = appendBuilder

instance Monoid (Builder a) where
  {-# INLINE mempty #-}
  mempty = BuilderStatic Text.empty
  {-# INLINE mappend #-}
  mappend = (Semigroup.<>)

fromText :: Text -> Builder a
fromText = BuilderStatic
{-# INLINE fromText #-}

contramapBuilder :: (b -> a) -> Builder a -> Builder b
contramapBuilder f x = case x of
  BuilderStatic t -> BuilderStatic t
  BuilderFunction t g -> BuilderFunction t (\ix marr b -> g ix marr (f b))
{-# INLINE contramapBuilder #-}

run :: Builder a -> a -> Text
run x = case x of
  BuilderStatic t -> \_ -> t
  BuilderFunction t f ->
    let (inArr, len) = I.portableUntext t
     in \a ->
          let outArr = runST $ do
                marr <- A.new len
                A.copyI marr 0 inArr 0 len
                f 0 marr a
                A.unsafeFreeze marr
           in TI.text outArr 0 len
{-# INLINE run #-}

word8HexFixedUpper :: Builder Word8
word8HexFixedUpper = word8HexFixedGeneral True
{-# INLINE word8HexFixedUpper #-}

word8HexFixedLower :: Builder Word8
word8HexFixedLower = word8HexFixedGeneral False
{-# INLINE word8HexFixedLower #-}

-- The Bool is True if the hex digits are upper case.
word8HexFixedGeneral :: Bool -> Builder Word8
word8HexFixedGeneral upper =
  BuilderFunction (Text.pack "--") $ \i marr w -> do
    let ix = unsafeShiftL (fromIntegral w) 1
        ix2 = ix + 1
        arr = if upper then I.hexValuesWord8Upper else I.hexValuesWord8Lower
    A.unsafeWrite marr i (A.unsafeIndex arr ix)
    A.unsafeWrite marr (i + 1) (A.unsafeIndex arr ix2)
{-# INLINE word8HexFixedGeneral #-}

-- | Characters outside the basic multilingual plane are not handled
--   correctly by this function. They will not cause a program to crash;
--   instead, the character will have the upper bits masked out.
charBmp :: Builder Char
charBmp =
  BuilderFunction (Text.pack "-") $ \i marr c -> A.unsafeWrite marr i (fromIntegral (ord c))
{-# INLINE charBmp #-}

word12HexFixedGeneral :: Bool -> Builder Word12
word12HexFixedGeneral upper =
  BuilderFunction (Text.pack "---") $ \i marr w -> do
    let !wInt = fromIntegral w
        !ix = wInt + wInt + wInt
        !arr = if upper then I.hexValuesWord12Upper else I.hexValuesWord12Lower
    A.unsafeWrite marr i (A.unsafeIndex arr ix)
    A.unsafeWrite marr (i + 1) (A.unsafeIndex arr (ix + 1))
    A.unsafeWrite marr (i + 2) (A.unsafeIndex arr (ix + 2))
{-# INLINE word12HexFixedGeneral #-}

word12HexFixedUpper :: Builder Word12
word12HexFixedUpper = word12HexFixedGeneral True
{-# INLINE word12HexFixedUpper #-}

word12HexFixedLower :: Builder Word12
word12HexFixedLower = word12HexFixedGeneral False
{-# INLINE word12HexFixedLower #-}

