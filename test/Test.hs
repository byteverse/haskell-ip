module Main (main) where

import Naive
import Data.List                            (intercalate)
import Test.QuickCheck                      (Gen, Arbitrary(..), choose)
import Test.Framework                       (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Net.Types (IPv4(..),IPv4Range(..))
import Net.Mac (Mac(..))
import qualified Net.IPv4 as IPv4
import qualified Net.IPv4.Range as IPv4Range
import qualified Net.IPv4.Text as IPv4_Text
import qualified Net.IPv4.ByteString.Char8 as IPv4_ByteString
import qualified Net.Mac as Mac
import qualified Net.Mac.Text as Mac_Text

import ArbitraryInstances ()
import qualified Naive
import qualified IPv4Text1
import qualified IPv4Text2
import qualified IPv4ByteString1

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Encoding and Decoding"
    [ testGroup "Naive IPv4 encode/decode"
      [ testProperty "Isomorphism"
          $ propEncodeDecodeIso Naive.encodeText Naive.decodeText
      ]
    , testGroup "Text Builder IPv4 Text encode/decode"
      [ testProperty "Identical to Naive"
          $ propMatching IPv4Text2.encode Naive.encodeText
      ]
    , testGroup "Raw byte array IPv4 Text encode/decode"
      [ testProperty "Identical to Naive"
          $ propMatching IPv4Text1.encode Naive.encodeText
      ]
    , testGroup "Raw byte array (without lookup table) IPv4 ByteString encode/decode"
      [ testProperty "Identical to Naive"
          $ propMatching IPv4ByteString1.encode Naive.encodeByteString
      ]
    , testGroup "Raw byte array (with lookup table) IPv4 ByteString encode/decode"
      [ testProperty "Identical to Naive"
          $ propMatching IPv4_ByteString.encode Naive.encodeByteString
      ]
    , testGroup "Raw byte array MAC Text encode/decode"
      [ testProperty "Isomorphism"
          $ propEncodeDecodeIso Mac_Text.encode Mac_Text.decode
      ]
    ]
  , testGroup "IP Range Operations"
    [ testProperty "Idempotence of normalizing IPv4 range"
        $ propIdempotence IPv4Range.normalize
    , testProperty "Normalize does not affect membership" propNormalizeMember
    , testProperty "Membership agrees with bounds" propMemberUpperLower
    , testProperty "Range contains self" propRangeSelf
    ]
  ]

propEncodeDecodeIso :: Eq a => (a -> b) -> (b -> Maybe a) -> a -> Bool
propEncodeDecodeIso f g a = g (f a) == Just a

propMatching :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
propMatching f g a = f a == g a

propIdempotence :: Eq a => (a -> a) -> a -> Bool
propIdempotence f a = f a == f (f a)

propNormalizeMember :: IPv4 -> IPv4Range -> Bool
propNormalizeMember i r = IPv4Range.member i r == IPv4Range.member i (IPv4Range.normalize r)

propMemberUpperLower :: IPv4 -> IPv4Range -> Bool
propMemberUpperLower i r =
  (i >= IPv4Range.lowerInclusive r && i <= IPv4Range.upperInclusive r) == IPv4Range.member i r

propRangeSelf :: IPv4Range -> Bool
propRangeSelf r = IPv4Range.member (ipv4RangeBase r) r == True

