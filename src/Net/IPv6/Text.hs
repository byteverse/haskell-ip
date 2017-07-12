{-# LANGUAGE BangPatterns #-}

module Net.IPv6.Text
  ( encode
  , parser
  ) where

import Prelude hiding (print)
import Net.Types (IPv6(..))
import Net.IPv6 (toWord16s)
import Data.Bits
import Data.List (intercalate, group)
import Data.Word
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Attoparsec.Text as Atto
import Numeric (showHex)

-- | Encodes the IP, using zero-compression on the leftmost-longest string of
-- zeroes in the address.
encode :: IPv6 -> Text
encode ip = toText [w1, w2, w3, w4, w5, w6, w7, w8]
  where
  (w1, w2, w3, w4, w5, w6, w7, w8) = toWord16s ip
  toText ws = Text.pack $ intercalate ":" $ expand 0 longestZ grouped
    where
    expand _ 8 _ = ["::"]
    expand _ _ [] = []
    expand i longest ((x, len):wsNext)
        -- zero-compressed group:
        | x == 0 && len == longest =
            -- first and last need an extra colon since there's nothing
            -- to concat against
            (if i == 0 || (i+len) == 8 then ":" else "")
            : expand (i+len) 0 wsNext
        -- normal group:
        | otherwise = replicate len (showHex x "") ++ expand (i+len) longest wsNext
    longestZ = maximum . (0:) . map snd . filter ((==0) . fst) $ grouped
    grouped = map (\x -> (head x, length x)) (group ws)

        
parser :: Atto.Parser IPv6
parser = do
  s <- start
  case toIPv6 s of
    Nothing -> fail "Wrong number of octets in IPv6 address"
    Just ip -> return ip
  where
  msg = "All chunks in a formatted IPv6 address must be between 0x0000 and 0xFFFF"
  colonMsg = "Cannot use double colon for omitting zeroes more than once in an IPv6 address"
  start = do
    c <- Atto.peekChar'
    if c == ':'
      then go (-1) 0 []
      else Atto.hexadecimal >>= \w -> go (-1) 1 [w]
    -- r <- fmap Just Atto.hexadecimal <|> (Nothing <$ Atto.char ':')
    -- case r of
    --   Just !w -> go (-1) 1 [w]
    --   Nothing -> go 0 0 []
  go !colonIndex !currentIndex !ws = do
    r <- do
      m <- Atto.peekChar
      case m of
        Nothing -> return ResDone
        Just c -> if c == ':'
          then do
            _ <- Atto.anyChar -- should be a colon
            if colonIndex == currentIndex
              then fmap ResWord Atto.hexadecimal <|> pure ResDone
              else do
                d <- Atto.peekChar'
                if d == ':'
                  then return ResColon
                  else fmap ResWord Atto.hexadecimal
          else return ResDone
    case r of
      ResDone -> pure (S colonIndex currentIndex ws)
      ResColon -> if alreadySet colonIndex
        then fail colonMsg
        else go currentIndex currentIndex ws
      ResWord w -> restrictTo16 msg w >> go colonIndex (currentIndex + 1) (w : ws)

toIPv6 :: S -> Maybe IPv6
toIPv6 (S colonIndex total input) = case compare total 8 of
  EQ -> if colonIndex == (-1)
    then go 0 0 input
    else Nothing
  GT -> Nothing
  LT -> go 0 0 input
  where
  revColonIndex = total - colonIndex
  spacesToSkip = 8 - total
  go :: Int -> Word64 -> [Word64] -> Maybe IPv6
  go !ix !acc ws = if ix > 3
    then fmap (flip IPv6 acc) (go2 ix 0 ws)
    else case ws of
      w : wsNext -> if ix == revColonIndex
        then go (ix + spacesToSkip) acc (w : wsNext)
        else go (ix + 1) (acc .|. unsafeShiftL w (times16 ix)) wsNext
      [] -> if ix == revColonIndex
        then Just $ IPv6 0 acc
        else Nothing -- Not enough word16s in list
  go2 :: Int -> Word64 -> [Word64] -> Maybe Word64
  go2 !ix !acc ws = case ws of
    w : wsNext -> if ix == revColonIndex
      then go2 (ix + spacesToSkip) acc (w : wsNext)
      else go2 (ix + 1) (acc .|. unsafeShiftL w (times16 ix - 64)) wsNext
    [] -> if ix == revColonIndex || ix > 7
      then Just acc
      else Nothing -- Not enough word16s in list

times16 :: Int -> Int
times16 a = unsafeShiftL a 4

alreadySet :: Int -> Bool
alreadySet i = i /= (-1)

restrictTo16 :: String -> Word64 -> Atto.Parser ()
restrictTo16 msg w = if w > 65535
  then fail msg
  else return ()

-- | This is an internal data type used as the result
--   after parsing an ipv6 address. The first field
--   indicates the index at which a double colon occurs.
--   The second is the length of the third.
--   The third is a reversed list of the 16s
--   that comprise the ipv6 address.
data S = S
  { _sDoubleColon :: {-# UNPACK #-} !Int
  , _sTotal :: {-# UNPACK #-} !Int
  , _sRevWords :: ![Word64]
  } deriving (Show,Read)

data Res
  = ResWord {-# UNPACK #-} !Word64
  | ResColon
  | ResDone


