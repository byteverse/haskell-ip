module Net.IPv6.Text
  ( parser
  )

import Prelude hiding (print)
import Net.Types (IPv6(..))
import qualified Data.Attoparsec.Text   as AT

parser :: Parser
parser = go False
  where
  go !alreadySkippedZeroes = AT.

