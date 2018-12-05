module Day03Spec where

import qualified Data.Text as T
import Test.Hspec

import Day03 (partOne)

spec :: Spec
spec = do

  describe "part one" $ do
    it "works for first example" $ do
      let fabricSize = 10
          claims = T.unlines $
            [ "#1 @ 1,3: 4x4"
            , "#2 @ 3,1: 4x4"
            , "#3 @ 5,5: 2x2"
            ]
      partOne fabricSize claims `shouldBe` "4"
