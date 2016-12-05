{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -O2 -Wall -funbox-strict-fields #-}

{-| For concatenating fixed-width strings that are only a few
    characters each, this can be ten times faster than the builder
    that ships with @text@. The restriction imposed is that all
    of the concatenated textual encoding be fixed-width.
-}
module Data.Text.SmallBuilder
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

import Data.Text.Internal (Text(..))
import Control.Monad.ST
import Data.Monoid
import Data.Word
import Data.Bits
import Text.Printf (printf)
import Debug.Trace
import Data.Char (ord)
import Data.Word.Synthetic (Word12)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Lazy.Builder.Int as TBuilder
import qualified Data.Text.IO as Text
import qualified Data.Text.Array as A

data Builder a where
  BuilderStatic :: !Text -> Builder a
  BuilderFunction :: !Text -> (forall s. Int -> A.MArray s -> a -> ST s ()) -> Builder a

instance Monoid (Builder a) where
  {-# INLINE mempty #-}
  mempty = BuilderStatic Text.empty
  {-# INLINE mappend #-}
  mappend x y = case x of
    BuilderStatic t1@(Text _ _ len1) -> case y of
      BuilderStatic t2 -> BuilderStatic (t1 <> t2)
      BuilderFunction t2 f -> BuilderFunction (t1 <> t2) (\ix marr a -> f (ix + len1) marr a)
    BuilderFunction t1@(Text _ _ len1) f1 -> case y of
      BuilderStatic t2 -> BuilderFunction (t1 <> t2) f1
      BuilderFunction t2 f2 -> BuilderFunction (t1 <> t2) (\ix marr a -> f1 ix marr a >> f2 (ix + len1) marr a)

fromText :: Text -> Builder a
fromText = BuilderStatic
{-# INLINE fromText #-}

contramapBuilder :: (b -> a) -> Builder a -> Builder b
contramapBuilder f x = case x of
  BuilderStatic t -> BuilderStatic t
  BuilderFunction t g -> BuilderFunction t (\ix marr b -> g ix marr (f b))
{-# INLINE contramapBuilder #-}

run :: Builder a -> a -> Text
run x a = case x of
  BuilderStatic t -> t
  BuilderFunction (Text inArr off len) f ->
    let outArr = A.run $ do
          marr <- A.new len
          A.copyI marr 0 inArr off len
          f 0 marr a
          return marr
     in Text outArr 0 len

word8HexFixedUpper :: Builder Word8
word8HexFixedUpper = word8HexFixedGeneral True
{-# INLINE word8HexFixedUpper #-}

-- | Lowercase fixed-width hexidecimal 'Word8' encoding. The text
--   produced is always two characters in length.
word8HexFixedLower :: Builder Word8
word8HexFixedLower = word8HexFixedGeneral False
{-# INLINE word8HexFixedLower #-}

-- The Bool is True if the hex digits are upper case.
word8HexFixedGeneral :: Bool -> Builder Word8
word8HexFixedGeneral upper = BuilderFunction (Text.pack "--") $ \i marr w -> do
  let ix = unsafeShiftL (fromIntegral w) 1
      ix2 = ix + 1
      arr = if upper then hexValuesUpper else hexValuesLower
  A.unsafeWrite marr i (A.unsafeIndex arr ix)
  A.unsafeWrite marr (i + 1) (A.unsafeIndex arr ix2)
{-# INLINE word8HexFixedGeneral #-}

-- | Characters outside the basic multilingual plane are not handled
--   correctly by this function. However, they will not cause a program to crash.
--   Instead, the character will have the upper bits masked out.
charBmp :: Builder Char
charBmp = BuilderFunction (Text.pack "-") $ \i marr c -> A.unsafeWrite marr i (fromIntegral (ord c))
{-# INLINE charBmp #-}

hexValuesUpper :: A.Array
hexValuesUpper = let Text arr _ _ = Text.copy $ Text.pack $ concat $ map (printf "%02X") [0 :: Int ..255] in arr
{-# NOINLINE hexValuesUpper #-}

hexValuesLower :: A.Array
hexValuesLower = let Text arr _ _ = Text.copy $ Text.pack $ concat $ map (printf "%02x") [0 :: Int ..255] in arr
{-# NOINLINE hexValuesLower #-}

word12HexFixedGeneral :: Bool -> Builder Word12
word12HexFixedGeneral upper = BuilderFunction (Text.pack "---") $ \i marr w -> do
  let !wInt = fromIntegral w
      !ix = wInt + wInt + wInt
      !arr = if upper then hexValuesWord12Upper else hexValuesWord12Lower
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

hexValuesWord12Upper :: A.Array
hexValuesWord12Upper = let Text arr _ _ = Text.copy $ Text.pack $ concat $ map (printf "%03X") [0 :: Int ..4096] in arr
{-# NOINLINE hexValuesWord12Upper #-}

hexValuesWord12Lower :: A.Array
hexValuesWord12Lower = let Text arr _ _ = Text.copy $ Text.pack $ concat $ map (printf "%03x") [0 :: Int ..4096] in arr
{-# NOINLINE hexValuesWord12Lower #-}

