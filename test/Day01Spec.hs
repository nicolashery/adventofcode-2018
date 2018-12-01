module Day01Spec where

import qualified Data.Text as T
import Test.Hspec

import Day01 (partOne)

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
