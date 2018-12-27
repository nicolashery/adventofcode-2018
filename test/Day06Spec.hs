module Day06Spec where

import qualified Data.Text as T
import Test.Hspec

import Day06 (partOne, partTwo)

spec :: Spec
spec = do
  let locations = T.unlines $
        [ "1, 1"
        , "1, 6"
        , "8, 3"
        , "3, 4"
        , "5, 5"
        , "8, 9"
        ]

  describe "part one" $ do
    it "works for first example" $ do
      partOne 10 locations `shouldBe` "17"

  describe "part two" $ do
    it "works for first example" $ do
      partTwo 10 32 locations `shouldBe` "16"
