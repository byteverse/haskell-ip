module Main (main) where

import Naive
import Criterion.Main
import Net.IPv4 (IPv4(..))
import qualified Net.IPv4.Text as IPv4_Text

main :: IO ()
main = do
  let ipAddr = IPv4 1000000009
  defaultMain 
    [ bgroup "IPv4 to Text" 
      [ bench "Naive" $ whnf ipv4ToTextNaive ipAddr
      , bench "Current Implementation" $ whnf IPv4_Text.encode ipAddr
      ]
    ]





