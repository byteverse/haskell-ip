module Main (main) where

import Naive
import Data.List (intercalate)
import Test.QuickCheck (Gen, Arbitrary(..), choose)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion,(@?=))
import Numeric (showHex)
import Test.QuickCheck.Property (failed,succeeded,Result(..))
import Data.Word
import Data.Bifunctor
import qualified Test.Framework.Providers.HUnit as PH

import Net.Types (IPv4(..),IPv4Range(..),Mac(..),IPv6(..))
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BC8
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6
import qualified Net.IPv4.Range as IPv4Range
import qualified Net.Mac as Mac
import qualified Net.Mac.Text as MacText
import qualified Net.Mac.ByteString.Char8 as MacByteString

import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.ByteString as AB

import ArbitraryInstances ()
import qualified Naive
import qualified IPv4Text1
import qualified IPv4Text2
import qualified IPv4ByteString1
-- import qualified IPv4TextVariableBuilder

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Encoding and Decoding"
    [ testGroup "Currently used IPv4 encode/decode" $
      [ testProperty "Isomorphism"
          $ propEncodeDecodeIso IPv4.encode IPv4.decode
      , PH.testCase "Decode an IP" testIPv4Decode
      ] ++ testDecodeFailures
    , testGroup "Currently used MAC Text encode/decode"
      [ testProperty "Isomorphism"
          $ propEncodeDecodeIsoSettings MacText.encodeWith MacText.decodeWith
      , PH.testCase "Encode a MAC Address" testMacEncode
      ]
    , testGroup "Currently used MAC ByteString encode/decode"
      [ testProperty "Isomorphism"
          $ propEncodeDecodeIsoSettings MacByteString.encodeWith MacByteString.decodeWith
      , PH.testCase "Lenient Decoding" testLenientMacByteStringParser
      ]
    , testGroup "Naive IPv4 encode/decode"
      [ testProperty "Isomorphism"
          $ propEncodeDecodeIso Naive.encodeText Naive.decodeText
      ]
    , testGroup "Text Builder IPv4 Text encode/decode"
      [ testProperty "Identical to Naive"
          $ propMatching IPv4Text2.encode Naive.encodeText
      ]
    -- , testGroup "Variable Text Builder IPv4 Text encode/decode"
    --   [ testProperty "Identical to Naive"
    --       $ propMatching IPv4TextVariableBuilder.encode Naive.encodeText
    --   ]
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
          $ propMatching IPv4.encodeUtf8 Naive.encodeByteString
      ]
    , testGroup "IPv4 encode/decode"
      [ PH.testCase "Parser Test Cases" testIPv4Parser
      ]
    , testGroup "IPv6 encode/decode"
      [ PH.testCase "Parser Test Cases" testIPv6Parser
      , PH.testCase "Encode test cases" testIPv6Encode
      , PH.testCase "Parser Failure Test Cases" testIPv6ParserFailure
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

propEncodeDecodeIsoSettings :: (Eq a,Show a,Show b,Show e)
  => (e -> a -> b) -> (e -> b -> Maybe a) -> e -> a -> Result
propEncodeDecodeIsoSettings f g e a =
  let fa = f e a
      gfa = g e fa
   in if gfa == Just a
        then succeeded
        else failure $ concat
          [ "env:     ", show e, "\n"
          , "x:       ", show a, "\n"
          , "f(x):    ", show fa, "\n"
          , "g(f(x)): ", show gfa, "\n"
          ]

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

testIPv4Decode :: Assertion
testIPv4Decode = IPv4.decode (Text.pack "124.222.255.0")
             @?= Just (IPv4.fromOctets 124 222 255 0)

testLenientMacByteStringParser :: Assertion
testLenientMacByteStringParser = do
  go 0xAB 0x12 0x0F 0x1C 0x88 0x79
     "AB:12:0F:1C:88:79"
  go 0xAB 0x12 0x0F 0x0C 0xAA 0x76
     "AB1-20F-0CA-A76"
  where
  go a b c d e f str =
    Just (HexMac (Mac.fromOctets a b c d e f))
    @?= fmap HexMac (MacByteString.decodeLenient (BC8.pack str))

testIPv4Parser :: Assertion
testIPv4Parser = do
  go 202 10 19 54 "202.10.19.54"
  go 10 202 96 25 "10.202.96.25"
  where
  go a b c d str =
    Right (IPv4.fromOctets a b c d)
    @?= (AB.parseOnly
          (IPv4.parserUtf8 <* AT.endOfInput)
          (BC8.pack str)
        )

testIPv6Parser :: Assertion
testIPv6Parser = do
  -- Basic test
  go 0xABCD 0x1234 0xABCD 0x1234 0xDCBA 0x4321 0xFFFF 0xE0E0
     "ABCD:1234:ABCD:1234:DCBA:4321:FFFF:E0E0"
  -- Tests that leading zeros can be omitted
  go 0x1234 0x5678 0x9ABC 0xDEF0 0x0123 0x4567 0x89AB 0xCDEF
     "1234:5678:9ABC:DEF0:123:4567:89AB:CDEF"
  -- Test that the IPv6 "any" abbreviation works
  go 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000
     "::"
  go 0x1623 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000
     "1623::"
  go 0x0000 0x0000 0x0000 0x0000 0x0000 0x0000 0xABCD 0x1234
     "::ABCD:1234"
  go 0xAAAA 0x0000 0x0000 0x0000 0x0000 0x0000 0xABCD 0x1234
     "AAAA::ABCD:1234"
  go 0xAAAA 0x0000 0x0000 0x0000 0xBBBB 0x0000 0xABCD 0x1234
     "AAAA::BBBB:0000:ABCD:1234"
  go 0xAAAA 0x0000 0x0000 0x0000 0xBBBB 0x0000 0xABCD 0x1234
     "AAAA:0000:0000:0000:BBBB::ABCD:1234"
  where
  go a b c d e f g h str =
    Right (HexIPv6 (IPv6.fromWord16s a b c d e f g h))
    @?= fmap HexIPv6
      (AT.parseOnly
        (IPv6.parser <* AT.endOfInput)
        (Text.pack str)
      )

testIPv6ParserFailure :: Assertion
testIPv6ParserFailure = do
  go "1111:2222:3333:4444:5555:6666::7777:8888"
  go "1111:2222:3333:4444:5555:6666:7777:8888:9999"
  go "1111:2222:3333:4444:5555:6666:7777:8888::9999"
  where
  go str =
    Left ()
    @?= bimap (\_ -> ()) HexIPv6
      (AT.parseOnly
        (IPv6.parser <* AT.endOfInput)
        (Text.pack str)
      )

testIPv6Encode :: Assertion
testIPv6Encode = do

    -- degenerate cases:
    "::" `roundTripsTo` "::"
    "1234::" `roundTripsTo` "1234::"
    "::1234" `roundTripsTo` "::1234"

    -- zero-compression works:
    "1234:1234:0000:0000:0000:0000:3456:3434" `roundTripsTo` "1234:1234::3456:3434"

    -- picks first case:
    "1234:0000:1234:0000:1234:0000:0123:1234" `roundTripsTo` "1234::1234:0:1234:0:123:1234"

    -- picks longest case:
    "1234:0000:1234:0000:0:0000:0123:1234" `roundTripsTo` "1234:0:1234::123:1234"

    -- can exclude all but first and last:
    "1234::1234" `roundTripsTo` "1234::1234"

    -- prefers leftmost part to zero-compress:
    "1:2:0:0:5::8" `roundTripsTo` "1:2::5:0:0:8"

    -- can work with no zeroes:
    "1:2:3:4:5:6:7:8" `roundTripsTo` "1:2:3:4:5:6:7:8"

    -- works with only first or last:
    "::2:3:4:5:6:7:8" `roundTripsTo` "::2:3:4:5:6:7:8"
    "1:2:3:4:5:6:7::" `roundTripsTo` "1:2:3:4:5:6:7::"

   where
   roundTripsTo s sExpected =
     case AT.parseOnly (IPv6.parser <* AT.endOfInput) (Text.pack s) of
        Right result -> IPv6.encode result @?= Text.pack sExpected
        Left failMsg -> fail failMsg -- parse shouldn't fail here

textBadIPv4 :: [String]
textBadIPv4 =
  [ "122.256.0.0"
  , "1.1.1."
  , ".1.1.1."
  , ".1.1.1"
  , "1.1..1.1"
  , "1.9.x.2"
  , "1.9.3"
  , "1.9"
  ]

testDecodeFailures :: [Test]
testDecodeFailures = flip map textBadIPv4 $ \str ->
  PH.testCase ("Should fail to decode [" ++ str ++ "]") $ IPv4.decode (Text.pack str) @?= Nothing

testMacEncode :: Assertion
testMacEncode = MacText.encode (Mac.fromOctets 0xFF 0x00 0xAB 0x12 0x99 0x0F)
            @?= Text.pack "ff:00:ab:12:99:0f"

failure :: String -> Result
failure msg = failed
  { reason = msg
  , theException = Nothing
  }

newtype HexMac = HexMac { getHexMac :: Mac }
  deriving (Eq)

instance Show HexMac where
  showsPrec _ (HexMac v) =
    let (a,b,c,d,e,f) = Mac.toOctets v
     in showHex a . showChar ':'
        . showHex b . showChar ':'
        . showHex c . showChar ':'
        . showHex d . showChar ':'
        . showHex e . showChar ':'
        . showHex f


newtype HexIPv6 = HexIPv6 { getHexIPv6 :: IPv6 }
  deriving (Eq)

instance Show HexIPv6 where
  showsPrec _ (HexIPv6 v) =
    let (a,b,c,d,e,f,g,h) = IPv6.toWord16s v
     in showHex a . showChar ':'
        . showHex b . showChar ':'
        . showHex c . showChar ':'
        . showHex d . showChar ':'
        . showHex e . showChar ':'
        . showHex f . showChar ':'
        . showHex g . showChar ':'
        . showHex h

newtype Hex a = Hex { getHex :: a }

-- instance (Integral a, Show a) => Show (Hex a) where
--   show = showHex . getHex


