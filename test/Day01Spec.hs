module Day01Spec where

import qualified Data.Text as T
import Test.Hspec

import Day01 (partOne, partTwo)

spec :: Spec
spec = do

  describe "part one" $ do
    it "works for first example" $ do
      partOne (T.unlines ["+1", "-2", "+3", "+1"]) `shouldBe` "3"
    it "works for second example" $ do
      partOne (T.unlines ["+1", "+1", "+1"]) `shouldBe` "3"
    it "works for third example" $ do
      partOne (T.unlines ["+1", "+1", "-2"]) `shouldBe` "0"
    it "works for fourth example" $ do
      partOne (T.unlines ["-1", "-2", "-3"]) `shouldBe` "-6"

  describe "part two" $ do
    it "works for first example" $ do
      partTwo (T.unlines ["+1", "-2", "+3", "+1"]) `shouldBe` "2"
    it "works for second example" $ do
      partTwo (T.unlines ["+1", "-1"]) `shouldBe` "0"
    it "works for third example" $ do
      partTwo (T.unlines ["+3", "+3", "+4", "-2", "-4"]) `shouldBe` "10"
    it "works for fourth example" $ do
      partTwo (T.unlines ["-6", "+3", "+8", "+5", "-6"]) `shouldBe` "5"
    it "works for fifth example" $ do
      partTwo (T.unlines ["+7", "+7", "-2", "-7", "-4"]) `shouldBe` "14"
