{-# OPTIONS_GHC -Wno-deprecations #-}
module Net.IPv4Spec (spec) where
import Prelude hiding (any)
import Data.Bits
import Net.IPv4
import Test.Hspec

spec :: Spec
spec = do
    describe "Bits" $ do
        it ".&." $ do
            any .&. any `shouldBe` any
            any .&. loopback `shouldBe` any
            loopback .&. broadcast `shouldBe` loopback
            broadcast .&. broadcast `shouldBe` broadcast
        it ".|." $ do
            any .|. any `shouldBe` any
            any .|. loopback `shouldBe` loopback
            loopback .|. broadcast `shouldBe` broadcast
            broadcast .|. broadcast `shouldBe` broadcast
        it "xor" $ do
            any `xor` any `shouldBe` any
            any `xor` loopback `shouldBe` loopback
            loopback `xor` broadcast `shouldBe` complement loopback
            broadcast `xor` broadcast `shouldBe` any
        it "complement" $ do
            complement any `shouldBe` broadcast
            complement loopback `shouldBe` ipv4 128 255 255 254
            complement broadcast `shouldBe` any
        it "shift" $ do
            shift any 0 `shouldBe` any
            shift broadcast 0 `shouldBe` broadcast
            shift broadcast 8 `shouldBe` ipv4 255 255 255 0
            shift broadcast (-8) `shouldBe` ipv4 0 255 255 255
            shift broadcast 32 `shouldBe` any
            shift broadcast 40 `shouldBe` any
        it "rotate" $ do
            rotate loopback 0 `shouldBe` loopback
            rotate loopback 0 `shouldBe` loopback
            rotate loopback 8 `shouldBe` ipv4 0 0 1 127
            rotate loopback (-8) `shouldBe` ipv4 1 127 0 0
            rotate loopback 32 `shouldBe` loopback
        it "bitSize" $ do
            bitSize any `shouldBe` 32
        it "bitSizeMaybe" $ do
            bitSizeMaybe any `shouldBe` Just 32
        it "isSigned" $ do
            isSigned any `shouldBe` False
            isSigned broadcast `shouldBe` False
        it "testBit" $ do
            testBit loopback <$> [0..31] `shouldBe`
              [ True, False, False, False, False, False, False, False
              , False, False, False, False, False, False, False, False
              , False, False, False, False, False, False, False, False
              , True, True, True, True, True, True, True, False ]
        it "bit" $ do
            bit 0 `shouldBe` ipv4 0 0 0 1
            bit 1 `shouldBe` ipv4 0 0 0 2
            bit 31 `shouldBe` ipv4 128 0 0 0
        it "popCount" $ do
            popCount any `shouldBe` 0
            popCount loopback `shouldBe` 8
            popCount broadcast `shouldBe` 32
    describe "FiniteBits" $ do
        it "finiteBitSize" $ do
            finiteBitSize any `shouldBe` 32
            finiteBitSize loopback `shouldBe` 32
            finiteBitSize broadcast `shouldBe` 32
