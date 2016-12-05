module IPv4TextVariableBuilder where

import Net.Types (IPv4(..))
import Data.Text (Text)
import Data.Monoid
import Data.Word
import Data.Bits
import qualified Net.IPv4 as IPv4
import qualified Data.Text as Text
import qualified Data.Text.Builder.Variable as VB

encode :: IPv4 -> Text
encode = VB.run variableBuilder

variableBuilder :: VB.Builder IPv4
variableBuilder =
  VB.contramap (word8At 24) VB.word8
  <> VB.staticCharBmp '.'
  <> VB.contramap (word8At 16) VB.word8
  <> VB.staticCharBmp '.'
  <> VB.contramap (word8At 8) VB.word8
  <> VB.staticCharBmp '.'
  <> VB.contramap (word8At 0) VB.word8

word8At :: Int -> IPv4 -> Word8
word8At i (IPv4 w) = fromIntegral (unsafeShiftR w i)
{-# INLINE word8At #-}



