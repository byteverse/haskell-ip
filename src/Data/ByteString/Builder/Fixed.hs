{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wall -funbox-strict-fields #-}

{-| For concatenating fixed-width strings that are only a few
    characters each, this can be six times faster than the builder
    that ships with @bytestring@.
-}
module Data.ByteString.Builder.Fixed
  ( Builder
  , fromByteString
  , run
  , contramapBuilder
  , char8
  , word8
  , word8HexFixedLower
  , word8HexFixedUpper
  , word12HexFixedLower
  , word12HexFixedUpper
  ) where

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif
import Data.Word
import Data.Word.Synthetic.Word12 (Word12)
import Data.Bits
import Data.Char (ord)
import Text.Printf
import Data.ByteString.Internal (ByteString(..))
import Foreign
import Data.ByteString.Short (ShortByteString)
import qualified Data.Semigroup as Semigroup
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.Primitive as PM

data Builder a where
  BuilderStatic :: !ByteString -> Builder a
  BuilderFunction :: !ByteString -> !(Int -> Ptr Word8 -> a -> IO ()) -> Builder a

{-# INLINE appendBuilder #-}
appendBuilder :: Builder a -> Builder a -> Builder a
appendBuilder x y = case x of
  BuilderStatic t1@(PS _ _ len1) -> case y of
    BuilderStatic t2 -> BuilderStatic (t1 <> t2)
    BuilderFunction t2 f -> BuilderFunction (t1 <> t2) (\ix marr a -> f (ix + len1) marr a)
  BuilderFunction t1@(PS _ _ len1) f1 -> case y of
    BuilderStatic t2 -> BuilderFunction (t1 <> t2) f1
    BuilderFunction t2 f2 -> BuilderFunction (t1 <> t2) (\ix marr a -> f1 ix marr a >> f2 (ix + len1) marr a)

instance Semigroup.Semigroup (Builder a) where
  {-# INLINE (<>) #-}
  (<>) = appendBuilder

instance Monoid (Builder a) where
  {-# INLINE mempty #-}
  mempty = BuilderStatic ByteString.empty
  {-# INLINE mappend #-}
  mappend = (Semigroup.<>)

contramapBuilder :: (b -> a) -> Builder a -> Builder b
contramapBuilder f x = case x of
  BuilderStatic t -> BuilderStatic t
  BuilderFunction t g -> BuilderFunction t (\ix marr b -> g ix marr (f b))
{-# INLINE contramapBuilder #-}

fromByteString :: ByteString -> Builder a
fromByteString = BuilderStatic
{-# INLINE fromByteString #-}

unsafeIndexShortByteString :: ShortByteString -> Int -> Word8
unsafeIndexShortByteString (SBS.SBS x) i = PM.indexByteArray (PM.ByteArray x) i
{-# INLINE unsafeIndexShortByteString #-}

run :: Builder a -> a -> ByteString
run x a = case x of
  BuilderStatic t -> t
  BuilderFunction (PS inArr off len) f ->
    BI.unsafeCreate len $ \ptr -> withForeignPtr inArr $ \inPtr -> do
      copyArray ptr (advancePtr inPtr off) len
      f 0 ptr a

word12HexFixedGeneral :: Bool -> Builder Word12
word12HexFixedGeneral upper = BuilderFunction (BC8.pack "---") $ \i marr w -> do
  let !wInt = fromIntegral w
      !ix = wInt + wInt + wInt
      !arr = if upper then hexValuesWord12Upper else hexValuesWord12Lower
  pokeByteOff marr i (unsafeIndexShortByteString arr ix)
  pokeByteOff marr (i + 1) (unsafeIndexShortByteString arr (ix + 1))
  pokeByteOff marr (i + 2) (unsafeIndexShortByteString arr (ix + 2))
{-# INLINE word12HexFixedGeneral #-}

word12HexFixedUpper :: Builder Word12
word12HexFixedUpper = word12HexFixedGeneral True
{-# INLINE word12HexFixedUpper #-}

word12HexFixedLower :: Builder Word12
word12HexFixedLower = word12HexFixedGeneral False
{-# INLINE word12HexFixedLower #-}

hexValuesWord12Upper :: ShortByteString
hexValuesWord12Upper =
  SBS.pack $ map (fromIntegral . ord) $ concat $ map (printf "%03X") [0 :: Int ..4095]
{-# NOINLINE hexValuesWord12Upper #-}

hexValuesWord12Lower :: ShortByteString
hexValuesWord12Lower =
  SBS.pack $ map (fromIntegral . ord) $ concat $ map (printf "%03x") [0 :: Int ..4095]
{-# NOINLINE hexValuesWord12Lower #-}

word8HexFixedUpper :: Builder Word8
word8HexFixedUpper = word8HexFixedGeneral True
{-# INLINE word8HexFixedUpper #-}

word8HexFixedLower :: Builder Word8
word8HexFixedLower = word8HexFixedGeneral False
{-# INLINE word8HexFixedLower #-}

-- The Bool is True if the hex digits are upper case.
word8HexFixedGeneral :: Bool -> Builder Word8
word8HexFixedGeneral upper = BuilderFunction (BC8.pack "--") $ \i marr w -> do
  let !ix = unsafeShiftL (fromIntegral w) 1
      !ix2 = ix + 1
      !arr = if upper then hexValuesWord8Upper else hexValuesWord8Lower
  pokeByteOff marr i (unsafeIndexShortByteString arr ix)
  pokeByteOff marr (i + 1) (unsafeIndexShortByteString arr ix2)
{-# INLINE word8HexFixedGeneral #-}

hexValuesWord8Upper :: ShortByteString
hexValuesWord8Upper =
  SBS.pack $ map (fromIntegral . ord) $ concat $ map (printf "%02X") [0 :: Int ..255]
{-# NOINLINE hexValuesWord8Upper #-}

hexValuesWord8Lower :: ShortByteString
hexValuesWord8Lower =
  SBS.pack $ map (fromIntegral . ord) $ concat $ map (printf "%02x") [0 :: Int ..255]
{-# NOINLINE hexValuesWord8Lower #-}

char8 :: Builder Char
char8 = BuilderFunction (BC8.pack "-") $ \i marr c -> pokeByteOff marr i (c2w c)
{-# INLINE char8 #-}

word8 :: Builder Word8
word8 = BuilderFunction (BC8.pack "-") $ \i marr w -> pokeByteOff marr i w
{-# INLINE word8 #-}

-- | Taken from @Data.ByteString.Internal@. The same warnings
--   apply here.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

-- macBuilder :: Builder Word64
-- macBuilder =
--      contramapBuilder (word8At 40) twoDigitWord8Hex
--   <> BuilderStatic ":"
--   <> contramapBuilder (word8At 32) twoDigitWord8Hex
--   <> BuilderStatic ":"
--   <> contramapBuilder (word8At 24) twoDigitWord8Hex
--   <> BuilderStatic ":"
--   <> contramapBuilder (word8At 16) twoDigitWord8Hex
--   <> BuilderStatic ":"
--   <> contramapBuilder (word8At 8) twoDigitWord8Hex
--   <> BuilderStatic ":"
--   <> contramapBuilder (word8At 0) twoDigitWord8Hex


