module Day07Spec where

import qualified Data.Text as T
import Test.Hspec

import Day07 (partOne, partTwo)

spec :: Spec
spec = do
  let requirements = T.unlines $
        [ "Step C must be finished before step A can begin."
        , "Step C must be finished before step F can begin."
        , "Step A must be finished before step B can begin."
        , "Step A must be finished before step D can begin."
        , "Step B must be finished before step E can begin."
        , "Step D must be finished before step E can begin."
        , "Step F must be finished before step E can begin."
        ]

  describe "part one" $ do
    it "works for first example" $ do
      partOne requirements `shouldBe` "CABDFE"

  describe "part two" $ do
    it "works for first example" $ do
      let workers = 2
          overhead = 0
      partTwo workers overhead requirements `shouldBe` "15"
