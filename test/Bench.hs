module Main (main) where

import Criterion.Main
import Net.Types (IPv4(..))
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Internal (Text(..))
import Data.Word
import Data.ByteString (ByteString)
import Control.Monad.ST
import qualified Data.Text              as Text
import qualified Data.ByteString.Char8  as BC8
import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Text.Lazy         as LText
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Array        as TArray
import qualified Net.IPv4.Text          as IPv4_Text

import qualified Naive
import qualified IPv4Text1
import qualified IPv4Text2
import qualified IPv4ByteString1
import qualified IPv4DecodeText1
import qualified IPv4DecodeText2

import qualified Net.IPv4.ByteString.Char8 as NIPBS

main :: IO ()
main = do
  let ipAddr = IPv4 1000000009
      ipText = Text.pack "192.168.5.99"
  defaultMain
    [ bgroup "IPv4 to Text"
      [ bench "Naive" $ whnf Naive.encodeText ipAddr
      , bench "Text Builder" $ whnf IPv4Text2.encode ipAddr
      , bench "Preallocated" $ whnf IPv4Text1.encode ipAddr
      ]
    , bgroup "IPv4 from Text"
      [ bench "Naive" $ whnf Naive.decodeText ipText
      , bench "Attoparsec" $ whnf IPv4DecodeText2.decodeText ipText
      , bench "Text Reader" $ whnf IPv4DecodeText1.decodeText ipText
      ]
    , bgroup "IPv4 to ByteString"
      [ bench "Naive" $ whnf Naive.encodeByteString ipAddr
      , bench "Preallocated: No Lookup Tables" $ whnf IPv4ByteString1.encode ipAddr
      , bench "Preallocated" $ whnf NIPBS.encode ipAddr
      ]
    ]
