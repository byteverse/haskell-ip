module Data.Text.Builder.Common.Internal where

import Control.Monad.ST
import Data.Char (ord)
import Data.Foldable (fold)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Unsafe.Char as TC
import Text.Printf

{- | This is slower that just pattern matching on the Text data constructor.
  However, it will work with GHCJS. This should only be used in places
  where we know that it will only be evaluated once.
-}
portableTextArray :: Text -> A.Array
portableTextArray = fst . portableUntext
{-# INLINE portableTextArray #-}

{- | This length is not the character length. It is the length of Word16s
  required by a UTF16 representation.
-}
portableTextLength :: Text -> Int
portableTextLength = snd . portableUntext
{-# INLINE portableTextLength #-}

portableUntext :: Text -> (A.Array, Int)
portableUntext t =
  let str = Text.unpack t
      Sum len = foldMap (Sum . charUtf16Size) str
      arr = A.run $ do
        marr <- A.new len
        writeString marr str
        return marr
   in (arr, len)
{-# NOINLINE portableUntext #-}

writeString :: A.MArray s -> String -> ST s ()
writeString marr = go 0
 where
  go i s = case s of
    c : cs -> do
      n <- TC.unsafeWrite marr i c
      go (i + n) cs
    [] -> return ()

charUtf16Size :: Char -> Int
charUtf16Size c = if ord c < 0x10000 then 1 else 2

hexValuesWord12Upper :: A.Array
hexValuesWord12Upper =
  portableTextArray $
    fold $
      map (Text.pack . printf "%03X") [0 :: Int .. 4096]
{-# NOINLINE hexValuesWord12Upper #-}

hexValuesWord12Lower :: A.Array
hexValuesWord12Lower =
  portableTextArray $
    fold $
      map (Text.pack . printf "%03x") [0 :: Int .. 4096]
{-# NOINLINE hexValuesWord12Lower #-}

hexValuesWord8Upper :: A.Array
hexValuesWord8Upper =
  portableTextArray $
    fold $
      map (Text.pack . printf "%02X") [0 :: Int .. 255]
{-# NOINLINE hexValuesWord8Upper #-}

hexValuesWord8Lower :: A.Array
hexValuesWord8Lower =
  portableTextArray $
    fold $
      map (Text.pack . printf "%02x") [0 :: Int .. 255]
{-# NOINLINE hexValuesWord8Lower #-}

twoDecimalDigits :: A.Array
twoDecimalDigits =
  portableTextArray $
    foldMap (Text.pack . printf "%02d") [0 :: Int .. 99]
{-# NOINLINE twoDecimalDigits #-}

threeDecimalDigits :: A.Array
threeDecimalDigits =
  portableTextArray $
    foldMap (Text.pack . printf "%03d") [0 :: Int .. 255]
{-# NOINLINE threeDecimalDigits #-}
