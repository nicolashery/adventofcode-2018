module Day02Spec where

import qualified Data.Text as T
import Test.Hspec

import Day02 (partOne)

spec :: Spec
spec = do

  describe "part one" $ do
    it "works for first example" $ do
      let boxes = T.unlines $
            [ "abcdef"
            , "bababc"
            , "abbcde"
            , "abcccd"
            , "aabcdd"
            , "abcdee"
            , "ababab"
            ]
      partOne boxes `shouldBe` "12"
