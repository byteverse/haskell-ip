{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main (main) where

import Naive
import Control.Applicative (liftA2)
import Data.Bytes (Bytes)
import Data.Proxy (Proxy(..))
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..),oneof,Gen,elements,choose,(===))
import Test.HUnit (Assertion,(@?=),(@=?))
import Numeric (showHex)
import Test.QuickCheck.Property (failed,succeeded,Result(..))
import Data.Bifunctor
import Test.QuickCheck.Classes (Laws(..),jsonLaws,showReadLaws,primLaws,boundedEnumLaws,bitsLaws)
import qualified Test.Tasty.HUnit as PH

import Net.Types (IP,IPv4(..),IPv4Range(..),Mac(..),IPv6(..),MacGrouping(..),MacCodec(..),IPv6Range(..))
import Data.WideWord (Word128(..))
import qualified Data.Bytes as Bytes
import qualified Data.Text as Text
import qualified Data.Text.Short as TS
import qualified Data.ByteString.Char8 as BC8
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6
import qualified Net.Mac as Mac
import qualified Net.IP as IP

import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.ByteString as AB

import qualified IPv4Text1
import qualified IPv4Text2
import qualified IPv4ByteString1
-- import qualified IPv4TextVariableBuilder

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ testGroup "Encoding and Decoding"
    [ testGroup "Currently used IPv4 encode/decode" $
      [ testProperty "Isomorphism"
          $ propEncodeDecodeIso IPv4.encode IPv4.decode
      , PH.testCase "Decode an IP" testIPv4Decode
      ] ++ testDecodeFailures
    , testGroup "Currently used IPv4 encodeShort/decodeShort" $
      [ testProperty "Isomorphism"
          $ propEncodeDecodeIso IPv4.encodeShort IPv4.decodeShort
      ] ++ testDecodeFailures
    , testGroup "Currently used IPv4 UTF-8 Bytes decode"
      [ testProperty "Isomorphism"
          $ propEncodeDecodeIso (byteStringToBytes . IPv4.encodeUtf8) IPv4.decodeUtf8Bytes
      , PH.testCase "Encode a MAC Address" testMacEncode
      ]
    , testGroup "Currently used MAC Text encode/decode"
      [ testProperty "Isomorphism"
          $ propEncodeDecodeIsoSettings Mac.encodeWith Mac.decodeWith
      , PH.testCase "Encode a MAC Address" testMacEncode
      ]
    , testGroup "Currently used MAC ByteString encode/decode"
      [ testProperty "Isomorphism"
          $ propEncodeDecodeIsoSettings Mac.encodeWithUtf8 Mac.decodeWithUtf8
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
      [ PH.testCase "Parser Test Cases" $ testIPv6Parser $ \str ->
          either (\_ -> Nothing) (Just . HexIPv6)
            (AT.parseOnly
              (IPv6.parser <* AT.endOfInput)
              (Text.pack str)
            )
      , PH.testCase "Bytes Parser Test Cases" $ testIPv6Parser $ \str ->
          fmap HexIPv6 (IPv6.decodeUtf8Bytes (Bytes.fromAsciiString str))
      , PH.testCase "Encode test cases" (testIPv6Encode IPv6.encode)
      , PH.testCase "Encode ShortText" (testIPv6Encode (TS.toText . IPv6.encodeShort))
      , PH.testCase "Parser Failure Test Cases"
          (testIPv6ParserFailure expectIPv6ParserFailure)
      , PH.testCase "Bytes Parser Failure Test Cases"
          (testIPv6ParserFailure expectIPv6BytesParserFailure)
      ]
    ]
  , testGroup "IPv4 Range Operations"
    [ testProperty "Idempotence of normalizing IPv4 range"
        $ propIdempotence IPv4.normalize
    , testProperty "Normalize does not affect membership" propNormalizeMember
    , testProperty "Membership agrees with bounds" propMemberUpperLower
    , testProperty "Range contains self" propRangeSelf
    , testGroup "reserved"
      [ PH.testCase "A" $ IPv4.reserved (IPv4.ipv4 0 1 2 3) @=? True
      , PH.testCase "B" $ IPv4.reserved (IPv4.ipv4 1 0 0 0) @=? False
      , PH.testCase "C" $ IPv4.reserved (IPv4.ipv4 100 64 0 3) @=? True
      , PH.testCase "D" $ IPv4.reserved (IPv4.ipv4 127 255 255 255) @=? True
      , PH.testCase "E" $ IPv4.reserved (IPv4.ipv4 110 0 0 255) @=? False
      , PH.testCase "F" $ IPv4.reserved (IPv4.ipv4 192 0 2 255) @=? True
      , PH.testCase "G" $ IPv4.reserved (IPv4.ipv4 203 0 113 0) @=? True
      , PH.testCase "H" $ IPv4.reserved (IPv4.ipv4 225 0 0 0) @=? True
      , PH.testCase "I" $ IPv4.reserved (IPv4.ipv4 226 0 0 0) @=? True
      , PH.testCase "J" $ IPv4.reserved (IPv4.ipv4 255 255 255 254) @=? True
      , PH.testCase "K" $ IPv4.reserved (IPv4.ipv4 255 255 255 255) @=? True
      , PH.testCase "L" $ IPv4.reserved (IPv4.ipv4 224 0 0 0) @=? True
      , PH.testCase "M" $ IPv4.reserved (IPv4.ipv4 239 255 255 255) @=? True
      , PH.testCase "N" $ IPv4.reserved (IPv4.ipv4 223 255 255 255) @=? False
      , PH.testCase "O" $ IPv4.reserved (IPv4.ipv4 203 0 114 0) @=? False
      , PH.testCase "P" $ IPv4.reserved (IPv4.ipv4 203 0 112 255) @=? False
      , PH.testCase "Q" $ IPv4.reserved (IPv4.ipv4 203 0 113 255) @=? True
      , PH.testCase "R" $ IPv4.reserved (IPv4.ipv4 192 88 100 0) @=? False
      , PH.testCase "S" $ IPv4.reserved (IPv4.ipv4 192 88 99 0) @=? True
      , PH.testCase "T" $ IPv4.reserved (IPv4.ipv4 192 0 1 0) @=? False
      ]
    , testGroup "private"
      [ PH.testCase "A" $ IPv4.private (IPv4.ipv4 198 73 8 38) @=? False
      , PH.testCase "B" $ IPv4.private (IPv4.ipv4 192 168 100 5) @=? True
      , PH.testCase "C" $ IPv4.private (IPv4.ipv4 10 0 0 0) @=? True
      , PH.testCase "D" $ IPv4.private (IPv4.ipv4 10 255 255 255) @=? True
      ]
    ]
  , testGroup "IPv6 Range Operations"
    [ testProperty "Idempotence of normalizing IPv6 range"
        $ propIdempotence IPv6.normalize
    , testProperty "Normalize does not affect membership" $ \i r ->
        IPv6.member i r == IPv6.member i (IPv6.normalize r)
    , testProperty "Membership agrees with bounds" $ \i r ->
        (i >= IPv6.lowerInclusive r && i <= IPv6.upperInclusive r) == IPv6.member i r
    , testProperty "Range contains self" $ \r ->
        IPv6.member (ipv6RangeBase r) r == True
    , testProperty "Idempotence of upperInclusive-lowerInclusive and fromBounds" $ \r ->
        IPv6.fromBounds (IPv6.lowerInclusive r) (IPv6.upperInclusive r) === r
    , testGroup "Cases"
      [ PH.testCase "A" $ False @=? IPv6.contains
          (IPv6.range (IPv6.ipv6 0 0 0 1 0 0 0 0) 64)
          (IPv6.ipv6 0 0 0 0 0 0 0 0)
      , PH.testCase "B" $ True @=? IPv6.contains
          (IPv6.range (IPv6.ipv6 0 0 0 0 0 0 0 0) 126)
          (IPv6.ipv6 0 0 0 0 0 0 0 1)
      , PH.testCase "C" $ False @=? IPv6.contains
          (IPv6.range (IPv6.ipv6 0 0 0 0 0 0 0 0) 125)
          (IPv6.ipv6 0 0 0 0 0 0 0 0xFFFF)
      ]
    ]
  , testGroup "Instances"
    [ testGroup "IPv4"
      [ lawsToTest (jsonLaws (Proxy :: Proxy IPv4))
      , lawsToTest (showReadLaws (Proxy :: Proxy IPv4))
       , lawsToTest (bitsLaws (Proxy :: Proxy IPv4))
      ]
    , testGroup "IPv4Range"
      [ lawsToTest (jsonLaws (Proxy :: Proxy IPv4Range))
      , lawsToTest (showReadLaws (Proxy :: Proxy IPv4Range))
      ]
    , testGroup "IPv6"
      [ lawsToTest (jsonLaws (Proxy :: Proxy IPv6))
      , lawsToTest (showReadLaws (Proxy :: Proxy IPv6))
      , lawsToTest (primLaws (Proxy :: Proxy IPv6))
      , lawsToTest (boundedEnumLaws (Proxy :: Proxy IPv6))
      , lawsToTest (bitsLaws (Proxy :: Proxy IPv6))
      ]
    , testGroup "IPv6Range"
      [ lawsToTest (jsonLaws (Proxy :: Proxy IPv6Range))
      , lawsToTest (showReadLaws (Proxy :: Proxy IPv6Range))
      ]
    , testGroup "IP"
      [ lawsToTest (jsonLaws (Proxy :: Proxy IP))
      , lawsToTest (showReadLaws (Proxy :: Proxy IP))
      ]
    , testGroup "Mac"
      [ lawsToTest (jsonLaws (Proxy :: Proxy Mac))
      , lawsToTest (showReadLaws (Proxy :: Proxy Mac))
      , lawsToTest (primLaws (Proxy :: Proxy Mac))
      ]
    ]
  ]

lawsToTest :: Laws -> TestTree
lawsToTest (Laws name pairs) = testGroup name (map (uncurry testProperty) pairs)

propEncodeDecodeIso :: (Eq a, Show a, Show b)
  => (a -> b) -> (b -> Maybe a) -> a -> Result
propEncodeDecodeIso f g a =
  let fa = f a
      gfa = g fa
   in if gfa == Just a
        then succeeded
        else failure $ concat
          [ "x:       ", show a, "\n"
          , "f(x):    ", show fa, "\n"
          , "g(f(x)): ", show gfa, "\n"
          ]

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
propNormalizeMember i r = IPv4.member i r == IPv4.member i (IPv4.normalize r)

propMemberUpperLower :: IPv4 -> IPv4Range -> Bool
propMemberUpperLower i r =
  (i >= IPv4.lowerInclusive r && i <= IPv4.upperInclusive r) == IPv4.member i r

propRangeSelf :: IPv4Range -> Bool
propRangeSelf r = IPv4.member (ipv4RangeBase r) r == True

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
    @=? fmap HexMac (Mac.decodeUtf8 (BC8.pack str))

testIPv4Parser :: Assertion
testIPv4Parser = do
  go 202 10 19 54 "202.10.19.54"
  go 10 202 96 25 "10.202.96.25"
  where
  go a b c d str =
    Right (IPv4.fromOctets a b c d)
    @=? (AB.parseOnly
          (IPv4.parserUtf8 <* AT.endOfInput)
          (BC8.pack str)
        )

testIPv6Parser :: (String -> Maybe HexIPv6) -> Assertion
testIPv6Parser decode = do
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
    Just (HexIPv6 (IPv6.fromWord16s a b c d e f g h))
    @=?
    decode str

testIPv6ParserFailure :: (String -> Assertion) -> Assertion
testIPv6ParserFailure go = do
  -- must not start or end in colon:
  go ":::"
  go "1::2:"
  go ":1::3"
  go "a:::"
  go ":b::"
  go "::c:"
  go "1:2:3:4:5:6:777:8:"
  go ":1:2:3:4:5:6:7777:8"

  -- Incorrect numbers of parts:
  go ""
  go "1111"
  go "1111:2222"
  go "1111:2222:3333"
  go "1111:2222:3333:4444"
  go "1111:2222:3333:4444:5555"
  go "1111:2222:3333:4444:5555:6666"
  go "1111:2222:3333:4444:5555:6666:7777"
  go "1111:2222:3333:4444:5555:6666:7777:8888:9999"

  -- Incorrect use of double-colon:
  go "1111::2222::3333"
  go "1111:2222:3333:4444:5555:6666::7777:8888" -- not needed
  go "1111:2222:3333:4444:5555:6666:7777:8888::9999" -- too long

  -- IPv4 decimal embedded, with not enough parts:
  go "1:127.0.0.1"
  go "1:2:3:127.0.0.1"
  go "1:2:3:4:127.0.0.1"
  go "1:2:3:4:5:127.0.0.1"

  -- IPv4 decimal before double-colon:
  go "1:127.0.0.1::"

  -- Only IPv4:
  go "127.0.0.1"

  -- IPv4 decimal embedded, with too many parts:
  go "1:2:3:4:5:6:7:127.0.0.1"
  go "1:2:3:4:5:6:7:8:127.0.0.1"

expectIPv6ParserFailure :: String -> Assertion
expectIPv6ParserFailure str =
  Left ()
  @=?
  bimap (\_ -> ()) HexIPv6
    (AT.parseOnly
      (IPv6.parser <* AT.endOfInput)
      (Text.pack str)
    )

expectIPv6BytesParserFailure :: String -> Assertion
expectIPv6BytesParserFailure s =
  Nothing
  @=?
  IPv6.decodeUtf8Bytes (Bytes.fromAsciiString s)

testIPv6Encode :: (IPv6 -> Text.Text) -> Assertion
testIPv6Encode enc = do

  -- degenerate cases:
  "::" `roundTripsTo` "::"
  "1234::" `roundTripsTo` "1234::"
  "::1234" `roundTripsTo` "::1234"

  -- zero-compression works:
  "1234:1234:0000:0000:0000:0000:3456:3434" `roundTripsTo` "1234:1234::3456:3434"

  -- picks first case:
  "1234:0000:1234:0000:1234:0000:0123:1234" `roundTripsTo` "1234:0:1234:0:1234:0:123:1234"

  -- picks longest case:
  "1234:0000:1234:0000:0:0000:0123:1234" `roundTripsTo` "1234:0:1234::123:1234"

  -- can exclude all but first and last:
  "1234::1234" `roundTripsTo` "1234::1234"

  -- prefers leftmost part to zero-compress:
  "1:2:0:0:5::8" `roundTripsTo` "1:2::5:0:0:8"

  -- can work with no zeroes:
  "1:2:3:4:5:6:7:8" `roundTripsTo` "1:2:3:4:5:6:7:8"

  -- works with only first or last:
  "::2:3:4:5:6:7:8" `roundTripsTo` "0:2:3:4:5:6:7:8"
  "1:2:3:4:5:6:7::" `roundTripsTo` "1:2:3:4:5:6:7:0"

  -- decimal notation in IPv6 addresses:
  "1:2:3:4:5:6:0.7.0.8" `roundTripsTo` "1:2:3:4:5:6:7:8"
  "::0.0.0.0" `roundTripsTo` "::"

  -- per https://tools.ietf.org/html/rfc5952#section-5
  "::ffff:0:0" `roundTripsTo` "::ffff:0.0.0.0"
  "::ffff:00ff:ff00" `roundTripsTo` "::ffff:0.255.255.0"
  "::ffff:203.0.113.17" `roundTripsTo` "::ffff:203.0.113.17"
  "1234:5678::10.0.1.2" `roundTripsTo` "1234:5678::a00:102"

 where
 roundTripsTo s sExpected =
   case AT.parseOnly (IPv6.parser <* AT.endOfInput) (Text.pack s) of
      Right result -> enc result @?= Text.pack sExpected
      Left failMsg -> fail ("failed to parse '" ++ s ++ "': " ++ failMsg)

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
  , "127.0.0.18446744073709551617"
  ]

testDecodeFailures :: [TestTree]
testDecodeFailures = flip map textBadIPv4 $ \str ->
  PH.testCase ("Should fail to decode [" ++ str ++ "]") $ IPv4.decode (Text.pack str) @?= Nothing

testMacEncode :: Assertion
testMacEncode = Mac.encode (Mac.fromOctets 0xFF 0x00 0xAB 0x12 0x99 0x0F)
            @?= Text.pack "ff:00:ab:12:99:0f"

failure :: String -> Result
failure msg = failed
  { reason = msg
  , theException = Nothing
  }

newtype HexMac = HexMac Mac
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


newtype HexIPv6 = HexIPv6 IPv6
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


deriving instance Arbitrary IPv4

instance Arbitrary Word128 where
  arbitrary = Word128 <$> arbitrary <*> arbitrary
  shrink (Word128 a b) = filter (/= Word128 a b)
    [ Word128 0 0
    , Word128 (div a 2) b
    , Word128 a (div b 2)
    ]

deriving instance Arbitrary IPv6

-- Half of the test cases generated are IPv6 mapped
-- IPv4 addresses.
instance Arbitrary IP where
  arbitrary = oneof
    [ IP.fromIPv4 <$> arbitrary
    , IP.fromIPv6 <$> arbitrary
    ]

instance Arbitrary Mac where
  arbitrary = Mac.fromOctets
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

-- This instance can generate masks that exceed the recommended
-- length of 32.
instance Arbitrary IPv4Range where
  arbitrary = IPv4.range <$> arbitrary <*> choose (0,32)

instance Arbitrary IPv6Range where
  arbitrary = IPv6.range <$> arbitrary <*> choose (0,128)
  shrink (IPv6Range addr mask) = liftA2 IPv6.range
    (shrink addr)
    (filter (/= mask) [0,div mask 2,if mask > 0 then mask - 1 else 0])

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

byteStringToBytes :: BC8.ByteString -> Bytes
byteStringToBytes = Bytes.fromAsciiString . BC8.unpack
