module Main (main) where

import Criterion.Main
import Net.Types (IPv4(..),MacGrouping(..),MacCodec(..))
import qualified Data.Text as Text
import qualified Net.Mac as Mac
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6
import qualified Data.Attoparsec.Text as Atto

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
      mac = Mac.fromOctets 0xFA 0xBB 0x43 0xA1 0x22 0x09
      ip6Text = Text.pack "::"
      ip6TextBigger = Text.pack "1:2:3:4:5:6:7:8"
      ip6TextSkip = Text.pack "1:2::7:8"
      ip6TextHex = Text.pack "a:b::c:d"
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
    , bgroup "IPv4 to ByteString"
      [ bench "Naive" $ whnf Naive.encodeByteString ipAddr
      , bench "Preallocated: No Lookup Tables" $ whnf IPv4ByteString1.encode ipAddr
      , bench "Preallocated" $ whnf IPv4.encodeUtf8 ipAddr
      ]
    , bgroup "IPv6 from Text"
      [ bench "New '::'" $ whnf (parseAll IPv6.parser) ip6Text
      , bench "New '1:2:3:4:5:6:7:8'" $ whnf (parseAll IPv6.parser) ip6TextBigger
      , bench "New '1:2::7:8'" $ whnf (parseAll IPv6.parser) ip6TextSkip
      , bench "New 'a:b::c:d'" $ whnf (parseAll IPv6.parser) ip6TextHex
      ]
    ]
  where 
  parseAll p = Atto.parseOnly (p <* Atto.endOfInput)
