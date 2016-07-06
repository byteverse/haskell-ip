{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Net.Types
  ( IPv4(..)
  , IPv4Range(..)
  , Mac(..)
  ) where

import qualified Net.Internal         as Internal
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as Aeson
import qualified Data.Attoparsec.Text as AT
import qualified Data.Vector.Generic            as GVector
import qualified Data.Vector.Unboxed            as UVector
import qualified Data.Vector.Primitive          as PVector
import qualified Data.Vector.Generic.Mutable    as MGVector
import qualified Data.Vector.Unboxed.Mutable    as MUVector
import qualified Data.Vector.Primitive.Mutable  as MPVector
import Data.Primitive.Types (Prim)
import Data.Bits ((.|.),shiftL)
import Data.Coerce (coerce)
import Control.Monad
import Data.Word
import Data.Int
import Data.Hashable
import Data.Aeson (FromJSON(..),ToJSON(..))
import GHC.Generics (Generic)

-- | A 32-bit Internet Protocol address.
newtype IPv4 = IPv4 { getIPv4 :: Word32 }
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Hashable,Generic,Prim)

-- | The length should be between 0 and 32. These bounds are inclusive.
--   This expectation is not in any way enforced by this library because
--   it does not cause errors. A mask length greater than 32 will be
--   treated as if it were 32.
data IPv4Range = IPv4Range
  { ipv4RangeBase   :: {-# UNPACK #-} !IPv4
  , ipv4RangeLength :: {-# UNPACK #-} !Word8
  } deriving (Eq,Ord,Show,Read,Generic)

instance Hashable IPv4Range

instance ToJSON IPv4 where
  toJSON (IPv4 addr) = Aeson.String (Internal.toDotDecimalText addr)

instance FromJSON IPv4 where
  parseJSON = Internal.attoparsecParseJSON
    (coerce (Internal.dotDecimalParser <* AT.endOfInput))

instance ToJSON IPv4Range where
  toJSON (IPv4Range (IPv4 addr) range) = Aeson.String (Internal.rangeToDotDecimalText addr range)

instance FromJSON IPv4Range where
  parseJSON (Aeson.String t) =
    case Internal.rangeFromDotDecimalText' mkIPv4Range t of
      Left err  -> fail err
      Right res -> return res
  parseJSON _ = mzero

mkIPv4Range :: Word32 -> Word8 -> IPv4Range
mkIPv4Range w len = IPv4Range (IPv4 w) len
{-# INLINE mkIPv4Range #-}

newtype instance UVector.MVector s IPv4 = MV_IPv4 (PVector.MVector s IPv4)
newtype instance UVector.Vector IPv4 = V_IPv4 (PVector.Vector IPv4)

instance UVector.Unbox IPv4

instance MGVector.MVector UVector.MVector IPv4 where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_IPv4 v) = MGVector.basicLength v
  basicUnsafeSlice i n (MV_IPv4 v) = MV_IPv4 $ MGVector.basicUnsafeSlice i n v
  basicOverlaps (MV_IPv4 v1) (MV_IPv4 v2) = MGVector.basicOverlaps v1 v2
  basicUnsafeNew n = MV_IPv4 `liftM` MGVector.basicUnsafeNew n
  basicInitialize (MV_IPv4 v) = MGVector.basicInitialize v
  basicUnsafeReplicate n x = MV_IPv4 `liftM` MGVector.basicUnsafeReplicate n x
  basicUnsafeRead (MV_IPv4 v) i = MGVector.basicUnsafeRead v i
  basicUnsafeWrite (MV_IPv4 v) i x = MGVector.basicUnsafeWrite v i x
  basicClear (MV_IPv4 v) = MGVector.basicClear v
  basicSet (MV_IPv4 v) x = MGVector.basicSet v x
  basicUnsafeCopy (MV_IPv4 v1) (MV_IPv4 v2) = MGVector.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_IPv4 v1) (MV_IPv4 v2) = MGVector.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_IPv4 v) n = MV_IPv4 `liftM` MGVector.basicUnsafeGrow v n

instance GVector.Vector UVector.Vector IPv4 where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_IPv4 v) = V_IPv4 `liftM` GVector.basicUnsafeFreeze v
  basicUnsafeThaw (V_IPv4 v) = MV_IPv4 `liftM` GVector.basicUnsafeThaw v
  basicLength (V_IPv4 v) = GVector.basicLength v
  basicUnsafeSlice i n (V_IPv4 v) = V_IPv4 $ GVector.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_IPv4 v) i = GVector.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_IPv4 mv) (V_IPv4 v) = GVector.basicUnsafeCopy mv v
  elemseq _ = seq

data Mac = Mac
  { macA :: {-# UNPACK #-} !Word16
  , macB :: {-# UNPACK #-} !Word32
  }
  deriving (Eq,Ord,Show,Read,Generic)

instance Hashable Mac

instance ToJSON Mac where
  toJSON (Mac a b) = Aeson.String (Internal.macToText a b)

instance FromJSON Mac where
  parseJSON = Internal.attoparsecParseJSON
    (Internal.macTextParser macFromOctets' <* AT.endOfInput)

macFromOctets' :: Word16 -> Word16 -> Word32 -> Word32 -> Word32 -> Word32 -> Mac
macFromOctets' a b c d e f = Mac
    ( shiftL a 8 .|. b )
    ( shiftL c 24 .|. shiftL d 16 .|. shiftL e 8 .|. f )
{-# INLINE macFromOctets' #-}
