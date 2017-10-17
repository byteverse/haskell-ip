{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Net.Types
  ( IPv4(..)
  , IPv6(..)
  , IP(..)
  , IPv4Range(..)
  , Mac(..)
  , MacCodec(..)
  , MacGrouping(..)
  ) where

import qualified Net.Internal         as Internal
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as Aeson
import qualified Data.Attoparsec.Text as AT
-- import qualified Data.Vector.Generic            as G
-- import qualified Data.Vector.Generic.Mutable    as M
import qualified Data.Vector.Generic            as GVector
import qualified Data.Vector.Unboxed            as UVector
import qualified Data.Vector.Primitive          as PVector
import qualified Data.Vector.Generic.Mutable    as MGVector
import qualified Data.Vector.Unboxed.Mutable    as MUVector
import qualified Data.Vector.Primitive.Mutable  as MPVector
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Builder as BB
import Foreign.Storable (Storable(..))
import Data.Primitive.Types (Prim)
import Data.Bits (Bits,FiniteBits,(.|.),unsafeShiftL)
import Data.Coerce (coerce)
import Control.Monad
import Data.Word
import Data.Int
import Data.Hashable
import Data.Aeson (FromJSON(..),ToJSON(..))
import GHC.Generics (Generic)
import Data.Monoid

#if MIN_VERSION_aeson(1,0,0) 
import qualified Data.Aeson.Encoding as Aeson
import Data.Aeson (ToJSONKey(..),FromJSONKey(..),
  ToJSONKeyFunction(..),FromJSONKeyFunction(..))
#endif

-- | A 32-bit Internet Protocol version 4 address.
newtype IPv4 = IPv4 { getIPv4 :: Word32 }
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Hashable,Generic,Prim,Bits,FiniteBits,Storable)

-- | A 128-bit Internet Protocol version 6 address.
data IPv6 = IPv6
  { ipv6A :: {-# UNPACK #-} !Word64
  , ipv6B :: {-# UNPACK #-} !Word64
  } deriving (Eq,Ord,Show,Read)

-- | A 32-bit 'IPv4' address or a 128-bit 'IPv6' address. Internally, this
--   is just represented as an 'IPv6' address. The functions provided
--   in @Net.IP@ help simulate pattern matching on it.
newtype IP = IP { getIP :: IPv6 }
  deriving (Eq,Ord,Show,Read)

-- | The length should be between 0 and 32. These bounds are inclusive.
--   This expectation is not in any way enforced by this library because
--   it does not cause errors. A mask length greater than 32 will be
--   treated as if it were 32.
data IPv4Range = IPv4Range
  { ipv4RangeBase   :: {-# UNPACK #-} !IPv4
  , ipv4RangeLength :: {-# UNPACK #-} !Word8
  } deriving (Eq,Ord,Show,Read,Generic)

-- | A 48-bit MAC address. Do not use the data constructor for this
--   type. It is not considered part of the stable API, and it
--   allows you to construct invalid MAC addresses.
newtype Mac = Mac { getMac :: Word64 }
  deriving (Eq,Ord,Show,Read,Generic)

-- data MacEncoding = MacEncoding
--   { macEncodingSeparator :: {-# UNPACK #-} !Word8 -- ^ ASCII value of the separator
--   , macEncodingUpperCase :: {-# UNPACK #-} !Bool
--   } deriving (Eq,Ord,Show,Read,Generic)

data MacCodec = MacCodec
  { macCodecGrouping :: !MacGrouping
  , macCodecUpperCase :: !Bool
  } deriving (Eq,Ord,Show,Read,Generic)

-- | The format expected by the mac address parser. The 'Word8' taken
--   by some of these constructors is the ascii value of the character
--   to be used as the separator. This is typically a colon, a hyphen, or
--   a space character. All decoding functions are case insensitive.
data MacGrouping
  = MacGroupingPairs !Char -- ^ Two-character groups, @FA:2B:40:09:8C:11@
  | MacGroupingTriples !Char -- ^ Three-character groups, @24B-F0A-025-829@
  | MacGroupingQuadruples !Char -- ^ Four-character groups, @A220.0745.CAC7@
  | MacGroupingNoSeparator -- ^ No separator, @24AF4B5B0780@
  deriving (Eq,Ord,Show,Read,Generic)

instance Hashable Mac

instance ToJSON Mac where
  toJSON (Mac w) = Aeson.String (Internal.macToTextDefault w)

#if MIN_VERSION_aeson(1,0,0) 
instance ToJSONKey Mac where
  toJSONKey = ToJSONKeyText
    (\(Mac w) -> Internal.macToTextDefault w)
    -- The bytestring encoding currently goes through text. This is suboptimal.
    (\(Mac w) -> Aeson.unsafeToEncoding $ BB.char7 '"' <> BB.byteString (TE.encodeUtf8 $ Internal.macToTextDefault w) <> BB.char7 '"')

instance FromJSONKey Mac where
  fromJSONKey = FromJSONKeyTextParser $ \t -> 
    case Internal.macFromText (Just ':') macFromOctets' t of
      Nothing -> fail "invalid mac address"
      Just mac -> return mac

instance ToJSONKey IPv4 where
  toJSONKey = ToJSONKeyText
    (\(IPv4 w) -> Internal.toDotDecimalText w)
    (\(IPv4 w) -> Aeson.unsafeToEncoding $ BB.char7 '"' <> (BB.byteString $ TE.encodeUtf8 $ Internal.toDotDecimalText w) <> BB.char7 '"')

instance FromJSONKey IPv4 where
  fromJSONKey = FromJSONKeyTextParser
    (Internal.eitherToAesonParser . coerce Internal.decodeIPv4TextEither)
#endif

instance FromJSON Mac where
  parseJSON = Internal.attoparsecParseJSON
    (Internal.macTextParser (Just ':') macFromOctets' <* AT.endOfInput)

-- Unchecked invariant: each of these Word64s must be smaller
-- than 256.
macFromOctets' :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Mac
macFromOctets' a b c d e f = 
  Mac (Internal.unsafeWord48FromOctets a b c d e f)
{-# INLINE macFromOctets' #-}

instance Hashable IPv4Range

instance ToJSON IPv4 where
  toJSON (IPv4 addr) = Aeson.String (Internal.toDotDecimalText addr)

instance FromJSON IPv4 where
  parseJSON = Aeson.withText "IPv4" (Internal.eitherToAesonParser . coerce Internal.decodeIPv4TextEither)

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

data instance MUVector.MVector s IPv4Range
    = MV_IPv4Range {-# UNPACK #-} !Int !(MUVector.MVector s IPv4)
                                       !(MUVector.MVector s Word8)
data instance UVector.Vector IPv4Range
    = V_IPv4Range {-# UNPACK #-} !Int !(UVector.Vector IPv4)
                                      !(UVector.Vector Word8)
instance UVector.Unbox IPv4Range
instance MGVector.MVector MUVector.MVector IPv4Range where
  {-# INLINE basicLength  #-}
  basicLength (MV_IPv4Range n_ as bs) = n_
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (MV_IPv4Range n_ as bs)
      = MV_IPv4Range m_ (MGVector.basicUnsafeSlice i_ m_ as)
                        (MGVector.basicUnsafeSlice i_ m_ bs)
  {-# INLINE basicOverlaps  #-}
  basicOverlaps (MV_IPv4Range n_1 as1 bs1) (MV_IPv4Range n_2 as2 bs2)
      = MGVector.basicOverlaps as1 as2
        || MGVector.basicOverlaps bs1 bs2
  {-# INLINE basicUnsafeNew  #-}
  basicUnsafeNew n_
      = do
          as <- MGVector.basicUnsafeNew n_
          bs <- MGVector.basicUnsafeNew n_
          return $ MV_IPv4Range n_ as bs
  {-# INLINE basicInitialize  #-}
  basicInitialize (MV_IPv4Range _ as bs)
      = do
          MGVector.basicInitialize as
          MGVector.basicInitialize bs
  {-# INLINE basicUnsafeReplicate  #-}
  basicUnsafeReplicate n_ (IPv4Range a b)
      = do
          as <- MGVector.basicUnsafeReplicate n_ a
          bs <- MGVector.basicUnsafeReplicate n_ b
          return $ MV_IPv4Range n_ as bs
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeRead (MV_IPv4Range n_ as bs) i_
      = do
          a <- MGVector.basicUnsafeRead as i_
          b <- MGVector.basicUnsafeRead bs i_
          return (IPv4Range a b)
  {-# INLINE basicUnsafeWrite  #-}
  basicUnsafeWrite (MV_IPv4Range n_ as bs) i_ (IPv4Range a b)
      = do
          MGVector.basicUnsafeWrite as i_ a
          MGVector.basicUnsafeWrite bs i_ b
  {-# INLINE basicClear  #-}
  basicClear (MV_IPv4Range n_ as bs)
      = do
          MGVector.basicClear as
          MGVector.basicClear bs
  {-# INLINE basicSet  #-}
  basicSet (MV_IPv4Range n_ as bs) (IPv4Range a b)
      = do
          MGVector.basicSet as a
          MGVector.basicSet bs b
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_IPv4Range n_1 as1 bs1) (MV_IPv4Range n_2 as2
                                                               bs2)
      = do
          MGVector.basicUnsafeCopy as1 as2
          MGVector.basicUnsafeCopy bs1 bs2
  {-# INLINE basicUnsafeMove  #-}
  basicUnsafeMove (MV_IPv4Range n_1 as1 bs1) (MV_IPv4Range n_2 as2
                                                               bs2)
      = do
          MGVector.basicUnsafeMove as1 as2
          MGVector.basicUnsafeMove bs1 bs2
  {-# INLINE basicUnsafeGrow  #-}
  basicUnsafeGrow (MV_IPv4Range n_ as bs) m_
      = do
          as' <- MGVector.basicUnsafeGrow as m_
          bs' <- MGVector.basicUnsafeGrow bs m_
          return $ MV_IPv4Range (m_+n_) as' bs'
instance GVector.Vector UVector.Vector IPv4Range where
  {-# INLINE basicUnsafeFreeze  #-}
  basicUnsafeFreeze (MV_IPv4Range n_ as bs)
      = do
          as' <- GVector.basicUnsafeFreeze as
          bs' <- GVector.basicUnsafeFreeze bs
          return $ V_IPv4Range n_ as' bs'
  {-# INLINE basicUnsafeThaw  #-}
  basicUnsafeThaw (V_IPv4Range n_ as bs)
      = do
          as' <- GVector.basicUnsafeThaw as
          bs' <- GVector.basicUnsafeThaw bs
          return $ MV_IPv4Range n_ as' bs'
  {-# INLINE basicLength  #-}
  basicLength (V_IPv4Range n_ as bs) = n_
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (V_IPv4Range n_ as bs)
      = V_IPv4Range m_ (GVector.basicUnsafeSlice i_ m_ as)
                       (GVector.basicUnsafeSlice i_ m_ bs)
  {-# INLINE basicUnsafeIndexM  #-}
  basicUnsafeIndexM (V_IPv4Range n_ as bs) i_
      = do
          a <- GVector.basicUnsafeIndexM as i_
          b <- GVector.basicUnsafeIndexM bs i_
          return (IPv4Range a b)
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_IPv4Range n_1 as1 bs1) (V_IPv4Range n_2 as2
                                                              bs2)
      = do
          GVector.basicUnsafeCopy as1 as2
          GVector.basicUnsafeCopy bs1 bs2
  {-# INLINE elemseq  #-}
  elemseq _ (IPv4Range a b)
      = GVector.elemseq (undefined :: UVector.Vector a) a
        . GVector.elemseq (undefined :: UVector.Vector b) b


-- newtype instance MUVector.MVector s Mac
--   = MV_Mac (MUVector.MVector s Word16)
-- 
-- newtype instance UVector.Vector Mac
--   = V_Mac (UVector.Vector Word16)
