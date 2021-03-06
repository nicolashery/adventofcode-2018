module Day08Spec where

import Test.Hspec

import Day08 (partOne, partTwo)

spec :: Spec
spec = do

  describe "part one" $ do
    it "works for first example" $ do
      partOne "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" `shouldBe` "138"

    it "works for second example" $ do
      partOne "3 3 1 2 0 2 1 9 2 9 2 2 1 3 0 2 3 9 4 9 9 0 2 5 9 6 9 0 3 7 9 9 8 9 9"
        `shouldBe` "135"

  describe "part two" $ do
    it "works for first example" $ do
      partTwo "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" `shouldBe` "66"
