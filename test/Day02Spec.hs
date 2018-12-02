module Day02Spec where

import qualified Data.Text as T
import Test.Hspec

import Day02 (partOne, partTwo)

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

  describe "part two" $ do
    it "works for first example" $ do
      let boxes = T.unlines $
            [ "abcde"
            , "fghij"
            , "klmno"
            , "pqrst"
            , "fguij"
            , "axcye"
            , "wvxyz"
            ]
      partTwo boxes `shouldBe` "fgij"
