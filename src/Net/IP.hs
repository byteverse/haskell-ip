module Net.IP
  ( -- * Pattern Matching
    case_
  , ipv4
  , ipv6
    -- * Construction
  , fromIPv4
  , fromIPv6
    -- * Textual Conversion
    -- ** Text
  , encode
  , decode
    -- * Types
  , IP(..)
  ) where

import Data.Bits
import Net.IPv6 (IPv6(..))
import Net.IPv4 (IPv4(..))
import Text.Read (Read(..),Lexeme(Ident),lexP,parens)
import Text.ParserCombinators.ReadPrec (prec,step,(+++))
import Data.Aeson (FromJSON(..),ToJSON(..))
import Data.Text (Text)
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6
import qualified Data.Aeson as Aeson

case_ :: (IPv4 -> a) -> (IPv6 -> a) -> IP -> a
case_ f g (IP addr@(IPv6 w1 w2)) = if w1 == 0 && (0xFFFFFFFF00000000 .&. w2 == 0x0000FFFF00000000)
  then f (IPv4 (fromIntegral w2))
  else g addr

-- | If the address is an 'IPv4' address, return the address.
ipv4 :: IP -> Maybe IPv4
ipv4 = case_ Just (const Nothing)

-- | If the address is an 'IPv6' address, and if it is not
--   an IPv4-mapped IPv6 address, return the address.
ipv6 :: IP -> Maybe IPv6
ipv6 = case_ (const Nothing) Just

fromIPv4 :: IPv4 -> IP
fromIPv4 (IPv4 w) = IP (IPv6 0 (0x0000FFFF00000000 .|. fromIntegral w))

fromIPv6 :: IPv6 -> IP
fromIPv6 = IP

encode :: IP -> Text
encode = case_ IPv4.encode IPv6.encode

decode :: Text -> Maybe IP
decode t = case IPv4.decode t of
  Nothing -> case IPv6.decode t of
    Nothing -> Nothing
    Just v6 -> Just (fromIPv6 v6)
  Just v4 -> Just (fromIPv4 v4)

-- | A 32-bit 'IPv4' address or a 128-bit 'IPv6' address. Internally, this
--   is just represented as an 'IPv6' address. The functions provided
--   in @Net.IP@ help simulate constructing and pattern matching on values
--   of this type. All functions and typeclass methods that convert
--   'IP' values to text will display it as an 'IPv4' address if possible.
newtype IP = IP { getIP :: IPv6 }
  deriving (Eq,Ord)

instance Show IP where
  showsPrec p = case_
    (\x -> showParen (p > 10) $ showString "fromIPv4 " . showsPrec 11 x)
    (\x -> showParen (p > 10) $ showString "fromIPv6 " . showsPrec 11 x)

instance Read IP where
  readPrec = parens $
    ( prec 10 $ do
        Ident "fromIPv4 " <- lexP
        fmap fromIPv4 (step readPrec)
    ) +++
    ( prec 10 $ do
        Ident "fromIPv6 " <- lexP
        fmap fromIPv6 (step readPrec)
    )

instance ToJSON IP where
  toJSON = Aeson.String . encode

instance FromJSON IP where
  parseJSON = Aeson.withText "IP" $ \t -> case decode t of
    Nothing -> fail "Could not parse IP address"
    Just addr -> return addr

