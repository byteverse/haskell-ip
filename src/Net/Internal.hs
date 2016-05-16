module Net.Internal where

import qualified Data.Attoparsec.Text as AT
import qualified Data.Aeson.Types as Aeson

attoparsecParseJSON :: AT.Parser a -> Aeson.Value -> Aeson.Parser a
attoparsecParseJSON p v = 
  case v of
    Aeson.String t -> 
      case AT.parseOnly p t of
        Left err  -> fail err
        Right res -> return res
    _ -> fail "expected a String"
