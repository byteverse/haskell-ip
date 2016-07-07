module IPv4DecodeText2 where

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

dotDecimalParser :: AT.Parser Word32
dotDecimalParser = fromOctets'
  <$> (AT.decimal >>= limitSize)
  <*  AT.char '.'
  <*> (AT.decimal >>= limitSize)
  <*  AT.char '.'
  <*> (AT.decimal >>= limitSize)
  <*  AT.char '.'
  <*> (AT.decimal >>= limitSize)
  where
  limitSize i =
    if i > 255
      then fail ipOctetSizeErrorMsg
      else return i

decodeText :: Text -> Maybe IPv4
decodeText t = case AT.parseOnly (dotDecimalParser <* AT.endOfInput) t of
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
