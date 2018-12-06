module Day03Spec where

import qualified Data.Text as T
import Test.Hspec

import Day03 (partOne, partTwo)

spec :: Spec
spec = do
  let fabricSize = 8
      claims = T.unlines $
        [ "#1 @ 1,3: 4x4"
        , "#2 @ 3,1: 4x4"
        , "#3 @ 5,5: 2x2"
        ]

  describe "part one" $ do
    it "works for first example" $ do
      partOne fabricSize claims `shouldBe` "4"

  describe "part two" $ do
    it "works for first example" $ do
      partTwo fabricSize claims `shouldBe` "3"
