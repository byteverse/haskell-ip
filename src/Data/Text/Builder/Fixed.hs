{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

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
import Text.Printf
import Debug.Trace
import Data.Char (ord)
import Data.Word.Synthetic (Word12)
import Data.Vector (Vector)
import Data.Foldable (fold)
import qualified Data.Vector as Vector
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Lazy.Builder.Int as TBuilder
import qualified Data.Text.IO as Text
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import qualified Data.Text.Internal.Unsafe.Char as TC

data Builder a where
  BuilderStatic :: Text -> Builder a
  BuilderFunction :: Text -> (forall s. Int -> A.MArray s -> a -> ST s ()) -> Builder a

instance Monoid (Builder a) where
  {-# INLINE mempty #-}
  mempty = BuilderStatic Text.empty
  {-# INLINE mappend #-}
  mappend x y = case x of
    BuilderStatic t1 -> case y of
      BuilderStatic t2 -> BuilderStatic (t1 <> t2)
      BuilderFunction t2 f ->
        let len1 = portableTextLength t1
         in BuilderFunction (t1 <> t2) (\ix marr a -> f (ix + len1) marr a)
    BuilderFunction t1 f1 -> case y of
      BuilderStatic t2 -> BuilderFunction (t1 <> t2) f1
      BuilderFunction t2 f2 ->
        let len1 = portableTextLength t1
         in BuilderFunction (t1 <> t2) (\ix marr a -> f1 ix marr a >> f2 (ix + len1) marr a)

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
  BuilderFunction t f ->
    let (inArr, len) = portableUntext t
        outArr = runST $ do
          marr <- A.new len
          A.copyI marr 0 inArr 0 len
          f 0 marr a
          A.unsafeFreeze marr
     in TI.text outArr 0 len

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
        arr = if upper then hexValuesWord8Upper else hexValuesWord8Lower
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

hexValuesWord12Upper :: A.Array
hexValuesWord12Upper = portableTextArray $ fold hexValuesWord12UpperTexts
{-# NOINLINE hexValuesWord12Upper #-}

hexValuesWord12Lower :: A.Array
hexValuesWord12Lower = portableTextArray $ fold hexValuesWord12LowerTexts
{-# NOINLINE hexValuesWord12Lower #-}

hexValuesWord8Upper :: A.Array
hexValuesWord8Upper = portableTextArray $ fold hexValuesWord8UpperTexts
{-# NOINLINE hexValuesWord8Upper #-}

hexValuesWord8Lower :: A.Array
hexValuesWord8Lower = portableTextArray $ fold hexValuesWord8LowerTexts
{-# NOINLINE hexValuesWord8Lower #-}

-- | This is slower that just pattern matching on the Text data constructor.
--   However, it will work with GHCJS. This should only be used is places
--   where we know that it will only be evaluated once.
portableTextArray :: Text -> A.Array
portableTextArray = fst . portableUntext

-- | This length is not the character length. It is the length of Word16s
--   required by a UTF16 representation.
portableTextLength :: Text -> Int
portableTextLength = snd . portableUntext

portableUntext :: Text -> (A.Array,Int)
portableUntext t =
  let str = Text.unpack t
      Sum len = foldMap (Sum . charUtf16Size) str
      arr = A.run $ do
        marr <- A.new len
        writeString marr str
        return marr
   in (arr,len)

writeString :: A.MArray s -> String -> ST s ()
writeString marr = go 0 where
  go i s = case s of
    c : cs -> do
      n <- TC.unsafeWrite marr i c
      go (i + n) cs
    [] -> return ()

charUtf16Size :: Char -> Int
charUtf16Size c = if ord c < 0x10000 then 1 else 2

