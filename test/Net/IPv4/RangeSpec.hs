{-# OPTIONS_GHC -Wno-deprecations #-}
module Net.IPv4.RangeSpec (spec) where
import Prelude hiding (any)
import Data.Bits
import Net.IPv4
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "Bits" $ do
    context "underlying IPv4 imlementation used correctly" $ do
      let host = range (ipv4 255 255 0 0) 32
          broadH = range broadcast 32
          negBroadH = range (ipv4 0 0 255 255) 32
      it ".&." $ do
        host .&. broadH `shouldBe` host
      it ".|." $ do
        host .|. broadH `shouldBe` broadH
      it "xor" $ do
        host `xor` broadH `shouldBe` negBroadH
      it "complement" $ do
        complement host `shouldBe` negBroadH
      it "shift" $ do
        shift host 8 `shouldBe` range (ipv4 255 0 0 0) 32
      it "rotate" $ do
        rotate host 8 `shouldBe` range (ipv4 255 0 0 255) 32
      it "isSigned" $ do
        isSigned host `shouldBe` False
    context "size operations use length correctly" $ do
      it "bitSize" $ do
          bitSize (range any 8) `shouldBe` 8
          bitSize (range any 15) `shouldBe` 15
          bitSize (range any 32) `shouldBe` 32
      it "bitSizeMaybe" $ do
          bitSizeMaybe (range broadcast 0) `shouldBe` Just 0
          bitSizeMaybe (range broadcast 24) `shouldBe` Just 24
          bitSizeMaybe (range broadcast 31) `shouldBe` Just 31
      it "testBit" $ do
        let prefix = range loopback 8
        testBit prefix <$> [0..31] `shouldBe`
          -- Note: final bit is False not True
          [ False, True,  True,  True,  True,  True,  True,  True
          , False, False, False, False, False, False, False, False
          , False, False, False, False, False, False, False, False
          , False, False, False, False, False, False, False, False]
      it "bit" $ do
        bit 0 `shouldBe` range (ipv4 128 0 0 0) 1
        bit 1 `shouldBe` range (ipv4 64 0 0 0) 2
        bit 31 `shouldBe` range (ipv4 0 0 0 1) 32
      it "popCount" $ do
        popCount (range any 0) `shouldBe` 0
        popCount (range broadcast 0) `shouldBe` 0
        popCount (range loopback 8) `shouldBe` 7
        popCount (range loopback 32) `shouldBe` 8
    context "operates on network bits only" $ do
      it "bitwise: same length" $ do
        (IPv4Range broadcast 16) .&. (IPv4Range broadcast 16)
          `shouldBe` (IPv4Range (ipv4 255 255 0 0) 16)
      it "bitwise: differing lengths ignoring host bits" $ do
        (IPv4Range broadcast 8) .&. (IPv4Range broadcast 16)
          `shouldBe` (IPv4Range (ipv4 255 0 0 0) 16)
      it "rebase: ignores host bits" $ do
        complement (IPv4Range loopback 16)
          `shouldBe` (IPv4Range (ipv4 128 255 0 0) 16)
  describe "FiniteBits" $ do
    it "finiteBitSize" $ do
      finiteBitSize . (range loopback) <$> [0..31] `shouldBe` [0..31]
