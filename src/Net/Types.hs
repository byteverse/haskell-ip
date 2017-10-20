{-# OPTIONS_GHC -Wall -Werror #-}

module Net.Types
  ( IPv4(..)
  , IPv6(..)
  , IP(..)
  , IPv4Range(..)
  , Mac(..)
  , MacCodec(..)
  , MacGrouping(..)
  ) where

import Net.IPv6 (IPv6(..))
import Net.IPv4 (IPv4(..))
import Net.IPv4.Range (IPv4Range(..))
import Net.IP (IP(..))
import Net.Mac (Mac(..),MacCodec(..),MacGrouping(..))

