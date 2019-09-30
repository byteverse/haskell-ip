module Main (main) where

import Criterion.Main
import Net.Types (IPv4(..),MacGrouping(..),MacCodec(..))
import Data.Maybe (fromJust)
import qualified Data.Bytes as Bytes
import qualified Data.Text as Text
import qualified Net.Mac as Mac
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6

import qualified Naive
import qualified IPv4Text1
import qualified IPv4Text2
import qualified IPv4ByteString1
import qualified IPv4DecodeText1
import qualified IPv4DecodeText2
-- import qualified IPv4TextVariableBuilder

main :: IO ()
main = do
  let ipAddr = IPv4 1000000009
      ipText = Text.pack "192.168.5.99"
      ipBytes = Bytes.fromAsciiString "192.168.5.99"
      mac = Mac.fromOctets 0xFA 0xBB 0x43 0xA1 0x22 0x09
      ip6Text = Text.pack "::"
      ip6 = fromJust $ IPv6.decode ip6Text
      ip6StrBigger = "1:2:3:4:5:6:7:8"
      ip6TextBigger = Text.pack ip6StrBigger
      ip6BytesBigger = Bytes.fromAsciiString "1:2:3:4:5:6:7:8"
      ip6Bigger = fromJust $ IPv6.decode ip6TextBigger
      ip6ComplicatedStr = "2001:db8:ba1:0:aaaa:542c:bb:cc00"
      ip6ComplicatedBytes = Bytes.fromAsciiString ip6ComplicatedStr
      ip6Complicated = fromJust $ IPv6.decode (Text.pack ip6ComplicatedStr)
      ip6TextSkip = Text.pack "1:2::7:8"
      ip6Skip = fromJust $ IPv6.decode ip6TextSkip
      ip6TextHex = Text.pack "a:b::c:d"
      ip6Hex = fromJust $ IPv6.decode ip6TextHex
  defaultMain
    [ bgroup "Mac to Text"
      [ bench "Current Implementation, pairs" $ whnf Mac.encode mac
      , bench "Current Implementation, no separator"
          $ whnf (Mac.encodeWith (MacCodec MacGroupingNoSeparator True)) mac
      , bench "Current Implementation, quads"
          $ whnf (Mac.encodeWith (MacCodec (MacGroupingQuadruples '-') True)) mac
      , bench "Current Implementation, triples"
          $ whnf (Mac.encodeWith (MacCodec (MacGroupingQuadruples '.') False)) mac
      ]
    , bgroup "Mac to ByteString"
      [ bench "Current Implementation, pairs" $ whnf Mac.encodeUtf8 mac
      , bench "Current Implementation, no separator"
          $ whnf (Mac.encodeWithUtf8 (MacCodec MacGroupingNoSeparator True)) mac
      ]
    , bgroup "IPv4 to ShortText"
      [ bench "Implementation" $ whnf IPv4.encodeShort ipAddr
      ]
    , bgroup "IPv4 to Text"
      [ bench "Naive" $ whnf Naive.encodeText ipAddr
      , bench "Text Builder" $ whnf IPv4Text2.encode ipAddr
      , bench "Preallocated" $ whnf IPv4Text1.encode ipAddr
      -- , bench "Variable Builder" $ whnf IPv4TextVariableBuilder.encode ipAddr
      ]
    , bgroup "IPv4 from Text"
      [ bench "Naive" $ whnf Naive.decodeText ipText
      , bench "Attoparsec" $ whnf IPv4DecodeText2.decodeText ipText
      , bench "Text Reader" $ whnf IPv4DecodeText1.decodeText ipText
      ]
    , bgroup "IPv4 from Bytes"
      [ bench "Current" $ whnf IPv4.decodeUtf8Bytes ipBytes
      ]
    , bgroup "IPv4 to ByteString"
      [ bench "Naive" $ whnf Naive.encodeByteString ipAddr
      , bench "Preallocated: No Lookup Tables" $ whnf IPv4ByteString1.encode ipAddr
      , bench "Preallocated" $ whnf IPv4.encodeUtf8 ipAddr
      ]
    , bgroup "IPv6 from Text"
      [ bench "::" $ whnf IPv6.decode ip6Text
      , bench "1:2:3:4:5:6:7:8" $ whnf IPv6.decode ip6TextBigger
      , bench "1:2::7:8" $ whnf IPv6.decode ip6TextSkip
      , bench "a:b::c:d" $ whnf IPv6.decode ip6TextHex
      ]
    , bgroup "IPv6 bytesmith"
      [ bench "1:2:3:4:5:6:7:8" $ whnf IPv6.decodeUtf8Bytes ip6BytesBigger
      , bench "2001:db8:ba1:0:aaaa:542c:bb:cc00" $ whnf IPv6.decodeUtf8Bytes ip6ComplicatedBytes
      ]
    , bgroup "IPv6 to Text"
      [ bench "::" $ whnf IPv6.encode ip6
      , bench "1:2:3:4:5:6:7:8" $ whnf IPv6.encode ip6Bigger
      , bench "1:2::7:8" $ whnf IPv6.encode ip6Skip
      , bench "a:b::c:d" $ whnf IPv6.encode ip6Hex
      ]
    , bgroup "IPv6 to ShortText"
      [ bench "1:2:3:4:5:6:7:8" $ whnf IPv6.encodeShort ip6Bigger
      , bench "1:2::7:8" $ whnf IPv6.encodeShort ip6Skip
      , bench "a:b::c:d" $ whnf IPv6.encodeShort ip6Hex
      , bench "2001:db8:ba1:0:aaaa:542c:bb:cc00" $ whnf IPv6.encodeShort ip6Complicated
      ]
    ]
