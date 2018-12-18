module Day07Spec where

import qualified Data.Text as T
import Test.Hspec

import Day07 (partOne)

spec :: Spec
spec = do

  describe "part one" $ do
    it "works for first example" $ do
      let requirements = T.unlines $
            [ "Step C must be finished before step A can begin."
            , "Step C must be finished before step F can begin."
            , "Step A must be finished before step B can begin."
            , "Step A must be finished before step D can begin."
            , "Step B must be finished before step E can begin."
            , "Step D must be finished before step E can begin."
            , "Step F must be finished before step E can begin."
            ]
      partOne requirements `shouldBe` "CABDFE"
