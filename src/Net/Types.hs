-- | This module re-exports all of the thematic types that this library defines.
module Net.Types
  ( IPv4 (..)
  , IPv6 (..)
  , IP (..)
  , IPv4Range (..)
  , IPv6Range (..)
  , Mac (..)
  , MacCodec (..)
  , MacGrouping (..)
  ) where

import Net.IP (IP (..))
import Net.IPv4 (IPv4 (..), IPv4Range (..))
import Net.IPv6 (IPv6 (..), IPv6Range (..))
import Net.Mac (Mac (..), MacCodec (..), MacGrouping (..))
