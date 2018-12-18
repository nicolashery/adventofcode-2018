module Day09Spec where

import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Test.Hspec

import Day09 (displayCircle, displayCircleSeq, partOne, partTwo, turn, turnSeq)

spec :: Spec
spec = do

  describe "turn" $ do
    it "works for normal marble" $ do
      let circle = [21,5,11,1,12,6,13,3,14,7,15,0,16,8,17,4,18,9,19,2,20,10]
          marble = 22
          circle' = fst $ turn (circle, M.empty) 1 marble
      displayCircle circle'
        `shouldBe`
        "0  16  8  17  4  18  9  19  2  20  10  21  5  (22)  11  1  12  6  13  3  14  7  15"

    it "works for multiple of 23 marble" $ do
        let circle = [22,11,1,12,6,13,3,14,7,15,0,16,8,17,4,18,9,19,2,20,10,21,5]
            marble = 23
            circle' = fst $ turn (circle, M.empty) 1 marble
        displayCircle circle'
          `shouldBe`
          "0  16  8  17  4  18  (19)  2  20  10  21  5  22  11  1  12  6  13  3  14  7  15"

  describe "turnSeq" $ do
    it "works for normal marble" $ do
      let circle = S.fromList [0,16,8,17,4,18,9,19,2,20,10,21,5,11,1,12,6,13,3,14,7,15]
          current = 11
          marble = 22
          (circle', current', _) = turnSeq (circle, current, M.empty) 1 marble
      displayCircleSeq circle' current'
        `shouldBe`
        "0  16  8  17  4  18  9  19  2  20  10  21  5  (22)  11  1  12  6  13  3  14  7  15"

    it "works for multiple of 23 marble" $ do
        let circle = S.fromList [0,16,8,17,4,18,9,19,2,20,10,21,5,22,11,1,12,6,13,3,14,7,15]
            current = 13
            marble = 23
            (circle', current', _) = turnSeq (circle, current, M.empty) 1 marble
        displayCircleSeq circle' current'
          `shouldBe`
          "0  16  8  17  4  18  (19)  2  20  10  21  5  22  11  1  12  6  13  3  14  7  15"

  describe "part one" $ do
    it "works for example in instructions " $ do
      partOne "9 players; last marble is worth 25 points" `shouldBe` "32"

    it "works for first example" $ do
      partOne "10 players; last marble is worth 1618 points" `shouldBe` "8317"

    it "works for second example" $ do
      partOne "17 players; last marble is worth 1104 points" `shouldBe` "2764"

  describe "part two" $ do
      it "works for example in instructions " $ do
        partTwo "9 players; last marble is worth 25 points" `shouldBe` "22563"
