{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Net.Types
  ( IPv4(..)
  , IPv4Range(..)
  ) where

import qualified Net.Internal         as Internal
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as Aeson
import qualified Data.Attoparsec.Text as AT
import Data.Coerce (coerce)
import Control.Monad
import Data.Word
import Data.Int
import Data.Hashable
import Data.Aeson (FromJSON(..),ToJSON(..))
import GHC.Generics (Generic)

-- | A 32-bit Internet Protocol address.
newtype IPv4 = IPv4 { getIPv4 :: Word32 }
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Hashable,Generic)

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

-- newtype instance UVector.MVector s IPv4 = MV_IPv4 (UVector.MVector s Word32)
--
-- instance MVector UVector.MVector IPv4 where
--   basicLength = coerce (basicLength :: UVector.MVector s Word32 -> Int)
--   basicUnsafeSlice = coerce (basicUnsafeSlice :: Int -> Int -> UVector.MVector s Word32 -> UVector.MVector s Word32)
--   basicInitialize :: forall m. PrimMonad m => UVector.MVector (PrimState m) IPv4 -> m ()
--   basicInitialize = coerce (basicInitialize :: PrimMonad m => UVector.MVector (PrimState m) Word32 -> m ())
--   basicUnsafeReplicate :: forall m. PrimMonad m => Int -> IPv4 -> m (UVector.MVector (PrimState m) IPv4)
--   basicUnsafeReplicate i (IPv4 w) = fmap coerce (basicUnsafeReplicate i w :: m (UVector.MVector (PrimState m) Word32))
--   basicUnsafeRead :: forall m. PrimMonad m => UVector.MVector (PrimState m) IPv4 -> Int -> m IPv4
--   basicUnsafeRead v i = fmap coerce (basicUnsafeRead (coerce v :: UVector.MVector (PrimState m) Word32) i :: m Word32)
--   basicUnsafeWrite :: forall m. PrimMonad m => UVector.MVector (PrimState m) IPv4 -> Int -> IPv4 -> m ()
--   basicUnsafeWrite = coerce (basicUnsafeWrite :: UVector.MVector (PrimState m) Word32 -> Int -> Word32 -> m ())
--   basicClear :: forall m. PrimMonad m => UVector.MVector (PrimState m) IPv4 -> m ()
--   basicClear = coerce (basicClear :: UVector.MVector (PrimState m) Word32 -> m ())
--   basicSet :: forall m. PrimMonad m => UVector.MVector (PrimState m) IPv4 -> IPv4 -> m ()
--   basicSet = coerce (basicSet :: UVector.MVector (PrimState m) Word32 -> Word32 -> m ())
--   basicUnsafeCopy :: forall m. PrimMonad m => UVector.MVector (PrimState m) IPv4 -> UVector.MVector (PrimState m) IPv4 -> m ()
--   basicUnsafeCopy = coerce (basicUnsafeCopy :: UVector.MVector (PrimState m) Word32 -> UVector.MVector (PrimState m) Word32 -> m ())
--   basicUnsafeMove :: forall m. PrimMonad m => UVector.MVector (PrimState m) IPv4 -> UVector.MVector (PrimState m) IPv4 -> m ()
--   basicUnsafeMove = coerce (basicUnsafeMove :: UVector.MVector (PrimState m) Word32 -> UVector.MVector (PrimState m) Word32 -> m ())
--   basicUnsafeGrow :: forall m. PrimMonad m => UVector.MVector (PrimState m) IPv4 -> Int -> m (UVector.MVector (PrimState m) IPv4)
--   basicUnsafeGrow (MV_IPv4 v) i = fmap coerce (basicUnsafeGrow v i)

