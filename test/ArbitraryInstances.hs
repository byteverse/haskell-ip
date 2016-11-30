{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ArbitraryInstances where

-- Orphan instances that are needed to make QuickCheck work.

import Net.Types (IPv4(..),IPv4Range(..),Mac(..),MacGrouping(..),MacCodec(..))
import Test.QuickCheck (Arbitrary(..),oneof,Gen,elements)
import Data.Word
import Data.Word.Synthetic

deriving instance Arbitrary IPv4
deriving instance Arbitrary Mac

-- This instance can generate masks that exceed the recommended
-- length of 32.
instance Arbitrary IPv4Range where
  arbitrary = fmap fromTuple arbitrary
    where fromTuple (a,b) = IPv4Range a b

instance Arbitrary Word48 where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Word64)

instance Arbitrary MacCodec where
  arbitrary = MacCodec <$> arbitrary <*> arbitrary

instance Arbitrary MacGrouping where
  arbitrary = oneof
    [ MacGroupingPairs <$> arbitraryMacSeparator
    , MacGroupingTriples <$> arbitraryMacSeparator
    , MacGroupingQuadruples <$> arbitraryMacSeparator
    , pure MacGroupingNoSeparator
    ]

arbitraryMacSeparator :: Gen Char
arbitraryMacSeparator = elements [':','-','.','_']

