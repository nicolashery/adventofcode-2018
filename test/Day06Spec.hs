module Day06Spec where

import qualified Data.Text as T
import Test.Hspec

import Day06 (partOne)

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
      partOne locations `shouldBe` "17"
