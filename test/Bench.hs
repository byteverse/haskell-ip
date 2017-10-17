module Main (main) where

import Criterion.Main
import Net.Types (IPv4(..),MacGrouping(..),MacCodec(..))
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Internal (Text(..))
import Data.Word
import Data.ByteString (ByteString)
import Control.Monad.ST
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Array as TArray
import qualified Net.Mac as Mac
import qualified Net.IPv4 as IPv4

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
    ]
