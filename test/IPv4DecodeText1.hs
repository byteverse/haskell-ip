module IPv4DecodeText1 where

import Net.Types
import Data.Monoid ((<>))
import Data.Word
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)
import Control.Monad.ST
import Data.Text.Internal (Text(..))
import Data.Text.Lazy.Builder.Int (decimal)
import Control.Monad
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as LText
import qualified Data.Attoparsec.Text   as AT
import qualified Data.Text.Array        as TArray
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Read         as TextRead
import qualified Data.Text.Lazy.Builder.Int as TBuilder

stripDecimal :: Text -> Either String Text
stripDecimal t = case Text.uncons t of
  Nothing -> Left "expected a dot but input ended instead"
  Just (c,tnext) -> if c == '.'
    then Right tnext
    else Left "expected a dot but found a different character"
{-# INLINE stripDecimal #-}

decodeIPv4TextEither :: Text -> Either String Word32
decodeIPv4TextEither t1' = do
  (a,t2) <- TextRead.decimal t1'
  t2' <- stripDecimal t2
  (b,t3) <- TextRead.decimal t2'
  t3' <- stripDecimal t3
  (c,t4) <- TextRead.decimal t3'
  t4' <- stripDecimal t4
  (d,t5) <- TextRead.decimal t4'
  when (not (Text.null t5)) (Left "expected end of text but it continued instead")
  if a > 255 || b > 255 || c > 255 || d > 255
    then Left ipOctetSizeErrorMsg
    else Right (fromOctets' a b c d)

decodeText :: Text -> Maybe IPv4
decodeText t = case decodeIPv4TextEither t of
  Left _ -> Nothing
  Right w -> Just (IPv4 w)

ipOctetSizeErrorMsg :: String
ipOctetSizeErrorMsg = "All octets in an IPv4 address must be between 0 and 255"

fromOctets' :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
fromOctets' a b c d =
    ( shiftL a 24
  .|. shiftL b 16
  .|. shiftL c 8
  .|. d
    )
