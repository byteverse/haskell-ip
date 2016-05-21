module Main (main) where

import Naive
import Criterion.Main
import Net.IPv4 (IPv4(..))
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Internal (Text(..))
import Data.Word
import Data.ByteString (ByteString)
import Control.Monad.ST
import qualified Data.ByteString.Char8  as BC8
import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Text.Lazy         as LText
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Array        as TArray
import qualified Net.IPv4.Text as IPv4_Text

main :: IO ()
main = do
  let ipAddr = IPv4 1000000009
  defaultMain 
    [ bgroup "IPv4 to Text" 
      [ bench "Naive" $ whnf ipv4ToTextNaive ipAddr
      , bench "Text Builder" $ whnf toDotDecimalText ipAddr
      , bench "Preallocated" $ whnf toTextPreAllocated ipAddr
      ]
    , bgroup "IPv4 to ByteString" 
      [ bench "Naive" $ whnf ipv4ToByteStringChar8Naive ipAddr
      ]
    ]
