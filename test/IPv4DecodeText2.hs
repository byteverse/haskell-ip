module IPv4DecodeText2 where

import Net.Types
import Data.Word
import Data.Bits (shiftL,(.|.))
import Data.Text.Internal (Text(..))
import qualified Data.Attoparsec.Text   as AT

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
