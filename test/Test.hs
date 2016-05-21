 {-# LANGUAGE StandaloneDeriving         #-}
 {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Naive
import Data.List                            (intercalate)
import Test.QuickCheck                      (Gen, Arbitrary(..), choose)
import Test.Framework                       (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Net.IPv4 (IPv4(..),IPv4Range(..))
import Net.Mac (Mac(..))
import qualified Net.IPv4 as IPv4
import qualified Net.IPv4.Text as IPv4_Text
import qualified Net.Mac as Mac
import qualified Net.Mac.Text as Mac_Text

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Naive IPv4 encode/decode" 
      [ testProperty "Isomorphism" 
          $ propEncodeDecodeIso ipv4ToTextNaive ipv4FromTextNaive
      ]
    , testGroup "Performant IPv4 Text encode/decode"
      [ testProperty "Isomorphism" 
          $ propEncodeDecodeIso IPv4_Text.encode IPv4_Text.decode
      , testProperty "Identical to Naive"
          $ propMatching IPv4_Text.encode ipv4ToTextNaive
      ]
    , testGroup "Performant MAC Text encode/decode"
      [ testProperty "Isomorphism" 
          $ propEncodeDecodeIso Mac_Text.encode Mac_Text.decode
      ]
    ]

deriving instance Arbitrary IPv4

instance Arbitrary Mac where
  arbitrary = fmap fromTuple arbitrary
    where fromTuple (a,b) = Mac a b

propEncodeDecodeIso :: Eq a => (a -> b) -> (b -> Maybe a) -> a -> Bool
propEncodeDecodeIso f g a = g (f a) == Just a

propMatching :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
propMatching f g a = f a == g a

