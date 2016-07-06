 {-# LANGUAGE DeriveGeneric #-}
 {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Net.Mac
  ( fromOctets
  , fromOctetsNoCast
  ) where

import Net.Types (Mac(..))
import Data.Text (Text)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.Word
import Data.Aeson (ToJSON(..),FromJSON(..))
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.ByteString.Char8 as AB
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)
import Net.Internal (attoparsecParseJSON,rightToMaybe)
import qualified Data.Text.Lazy.Builder as TBuilder
import Data.Text.Lazy.Builder.Int (hexadecimal)
import Data.Monoid ((<>))
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy as LText

fromOctets :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Mac
fromOctets a b c d e f = fromOctetsNoCast
  (fromIntegral a) (fromIntegral b) (fromIntegral c)
  (fromIntegral d) (fromIntegral e) (fromIntegral f)

fromOctetsNoCast :: Word16 -> Word16 -> Word32 -> Word32 -> Word32 -> Word32 -> Mac
fromOctetsNoCast a b c d e f = Mac
    ( shiftL a 8 .|. b )
    ( shiftL c 24 .|. shiftL d 16 .|. shiftL e 8 .|. f )
{-# INLINE fromOctetsNoCast #-}


