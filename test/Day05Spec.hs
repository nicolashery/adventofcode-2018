module Day05Spec where

import Test.Hspec

import Day05 (partOne, partTwo)

spec :: Spec
spec = do

  describe "part one" $ do
    it "works for first example" $ do
      partOne "dabAcCaCBAcCcaDA" `shouldBe` "10"

  describe "part one" $ do
    it "works for first example" $ do
      partTwo "dabAcCaCBAcCcaDA" `shouldBe` "4"
