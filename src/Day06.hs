module Day06 (partOne) where

import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Text.Parsec (ParseError, many1, parse)
import Text.Parsec.Char (digit, string)
import Text.Parsec.Text (Parser)

type Point = (Int, Int)

pointParser :: Parser Point
pointParser = do
  x <- int
  _ <- string ", "
  y <- int
  return (x, y)
  where
    int :: Parser Int
    int = read <$> many1 digit

parsePoint :: Text -> Point
parsePoint = onLeftError . parse pointParser ""
  where
    onLeftError :: Either ParseError Point -> Point
    onLeftError = either (error . show) id

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

coordinateArea :: [Point] -> Point -> Maybe Int
coordinateArea allCoordinates coordinate = go 1 0
  where
    go 100 _ = Nothing
    go r a =
      if da > 0
      then go (r + 1) (a + da)
      else Just (a + 1)
      where
        da = length $ filter isClosestToCoordinate (circle coordinate r)
        isClosestToCoordinate circlePoint = foldr f True allCoordinates
          where
            f _ False = False
            f otherCoordinate _ =
              (otherCoordinate == coordinate) ||
              distanceToCoordinate <
                (manhattanDistance circlePoint otherCoordinate)
            distanceToCoordinate = manhattanDistance circlePoint coordinate

circle :: Point -> Int -> [Point]
circle (cX, cY) r =
  [(cX + dx, cY + dy) | dx <- [-r..r], dy <- [-r..r], abs dx + abs dy == r]

partOne' :: [Point] -> Int
partOne' coordinates =
  foldr max 0 . catMaybes $ map (coordinateArea coordinates) coordinates

partOne :: Text -> Text
partOne = pack . show . partOne' . map parsePoint . T.lines
