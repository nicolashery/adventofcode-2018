module Day01Spec where

import qualified Data.Text as T
import Test.Hspec

import Day01 (solve)

spec :: Spec
spec = do

  describe "first puzzle" $ do
      it "works for first example" $ do
        solve (T.unlines ["+1", "-2", "+3", "+1"]) `shouldBe` "3"
      it "works for second example" $ do
        solve (T.unlines ["+1", "+1", "+1"]) `shouldBe` "3"
      it "works for third example" $ do
        solve (T.unlines ["+1", "+1", "-2"]) `shouldBe` "0"
      it "works for fourth example" $ do
        solve (T.unlines ["-1", "-2", "-3"]) `shouldBe` "-6"
