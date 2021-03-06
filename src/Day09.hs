module Day09
  (partOne
  , partTwo
  , displayCircle
  , displayCircleSeq
  , turn
  , turnSeq
  ) where

import Data.Foldable (toList)
import Data.List (break, cycle, foldl', intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text, pack)
import Text.Parsec (ParseError, many1, optional, parse)
import Text.Parsec.Char (digit, newline, string)
import Text.Parsec.Text (Parser)

type Player = Int
type Marble = Int
type Score = Int
type Circle = [Marble] -- Current marble is head of list
type GameState = (Circle, Map Player Score)

data Input = Input
  { inputNumberOfPlayers :: Int
  , inputNumberOfMarbles :: Marble
  } deriving (Eq, Show)

inputParser :: Parser Input
inputParser = do
  numberOfPlayers <- read <$> many1 digit
  _ <- string  " players; last marble is worth "
  numberOfMarbles <- read <$> many1 digit
  _ <- string " points"
  _ <- optional newline
  return $ Input
    { inputNumberOfPlayers = numberOfPlayers
    , inputNumberOfMarbles = numberOfMarbles
    }

newGame :: GameState
newGame = ([0], M.empty)

displayCircle :: Circle -> String
displayCircle [] = ""
displayCircle (current:rest) =
  let (left, right) = break (== 0) rest
  in intercalate "  " $ concat $
      [ map show right
      , ["(" ++ show current ++ ")"]
      , map show left
      ]

turn :: GameState -> Player -> Marble -> GameState
turn s p m
  | m `mod` 23 == 0 = turnTwentyThree s p m
  | otherwise = turnNormal s p m

turnNormal :: GameState -> Player -> Marble -> GameState
turnNormal (circle, scores) _ marbleToPlace = (circle', scores)
  where
    circle' :: Circle
    circle' = take (length circle + 1) $ marbleToPlace : (drop 2 clockwiseLoop)

    clockwiseLoop :: Circle
    clockwiseLoop = cycle circle

turnTwentyThree :: GameState -> Player -> Marble -> GameState
turnTwentyThree (circle, scores) player marbleToPlace = (circle', scores')
  where
    circle' :: Circle
    circle' =
      reverse $
        (take (length circle - 7) $
        drop 7 counterClockwiseLoop) ++ take 6 counterClockwiseLoop

    scores' :: Map Player Score
    scores' = M.insertWith (+) player (marbleToPlace + marbleToTakeOut) scores

    counterClockwiseLoop :: Circle
    counterClockwiseLoop = cycle $ reverse circle

    marbleToTakeOut :: Marble
    marbleToTakeOut = head $ reverse $ take 7 $ counterClockwiseLoop

play :: Int -> Int -> GameState
play numberOfPlayers numberOfMarbles =
  foldl' turn' newGame $ zip (cycle [1..numberOfPlayers]) [1..numberOfMarbles]
  where
    turn' s (p, m) = turn s p m

type CircleSeq = Seq Marble
type GameStateSeq = (CircleSeq, Int, Map Player Score)

newGameSeq :: GameStateSeq
newGameSeq = (S.fromList [0], 0, M.empty)

displayCircleSeq :: CircleSeq -> Int -> String
displayCircleSeq circle current =
  intercalate "  " $ toList $ S.mapWithIndex f circle
  where
    f :: Int -> Marble -> String
    f i marble
      | i == current = "(" ++ show marble ++ ")"
      | otherwise = show marble

turnSeq :: GameStateSeq -> Player -> Marble -> GameStateSeq
turnSeq s p m
  | m `mod` 23 == 0 = turnTwentyThreeSeq s p m
  | otherwise = turnNormalSeq s p m

turnNormalSeq :: GameStateSeq -> Player -> Marble -> GameStateSeq
turnNormalSeq (circle, current, scores) _ marbleToPlace =
  (circle', current', scores)
  where
    circle' :: CircleSeq
    circle' = S.insertAt current' marbleToPlace circle

    current' :: Int
    current' = (current + 1) `mod` (length circle) + 1

turnTwentyThreeSeq :: GameStateSeq -> Player -> Marble -> GameStateSeq
turnTwentyThreeSeq (circle, current, scores) player marbleToPlace =
  (circle', current', scores')
  where
    circle' :: CircleSeq
    circle' = S.deleteAt indexToTakeOut circle

    current' :: Int
    current' = indexToTakeOut `mod` (length circle - 1)

    scores' :: Map Player Score
    scores' = M.insertWith (+) player (marbleToPlace + marbleToTakeOut) scores

    marbleToTakeOut :: Marble
    marbleToTakeOut = S.index circle indexToTakeOut

    indexToTakeOut :: Int
    indexToTakeOut = (current - 7) `mod` (length circle)

playSeq :: Int -> Int -> GameStateSeq
playSeq numberOfPlayers numberOfMarbles =
  foldl' turn' newGameSeq $ zip (cycle [1..numberOfPlayers]) [1..numberOfMarbles]
  where
    turn' s (p, m) = turnSeq s p m

parseInput :: Text -> Input
parseInput input = onLeftError $ parse inputParser "" input
  where
    onLeftError :: Either ParseError Input -> Input
    onLeftError = either (error . show) id

partOne' :: Input -> Score
partOne' input = M.foldr max 0 scores
  where
    scores =
      snd $ play (inputNumberOfPlayers input) (inputNumberOfMarbles input)

partOne :: Text -> Text
partOne = pack . show . partOne' . parseInput

partTwo' :: Input -> Score
partTwo' input =
  let (_, _, scores) =
        playSeq (inputNumberOfPlayers input) (100 * inputNumberOfMarbles input)
  in M.foldr max 0 scores

partTwo :: Text -> Text
partTwo = pack . show . partTwo' . parseInput
