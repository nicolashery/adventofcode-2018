module Day06 (partOne, partTwo) where

import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
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

coordinateArea :: Int -> [Point] -> Point -> Maybe Int
coordinateArea maxSearchRadius allCoordinates coordinate = go 1 0
  where
    go r a
      | r == maxSearchRadius = Nothing
      | otherwise =
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

totalDistance :: [Point] -> Point -> Int
totalDistance allCoordinates location = foldr f 0 allCoordinates
  where
    f otherCoordinate total =
      total + manhattanDistance otherCoordinate location

computeRegionAroundCoordinate
  :: Int
  -> Int
  -> [Point]
  -> Point
  -> Set Point
  -> Set Point
computeRegionAroundCoordinate
  maxSearchRadius
  maxTotalDistance
  allCoordinates
  coordinate
  region
  = go 0 region
  where
    go r s
      | r == maxSearchRadius = s
      | otherwise =
          if S.null ds
          then s
          else go (r + 1) (S.union s ds)
          where
            ds =
              S.fromList $
                filter (flip S.notMember $ s) $
                filter isWithinRegion $
                circle coordinate r
            isWithinRegion circlePoint =
              totalDistance allCoordinates circlePoint < maxTotalDistance

partOne' :: Int -> [Point] -> Int
partOne' maxSearchRadius coordinates =
  foldr max 0 . catMaybes $
    map (coordinateArea maxSearchRadius coordinates) coordinates

partTwo' :: Int -> Int -> [Point] -> Int
partTwo' maxSearchRadius maxTotalDistance coordinates =
  S.size $ foldr computeRegionAroundCoordinate' S.empty coordinates
  where
    computeRegionAroundCoordinate' =
      computeRegionAroundCoordinate
        maxSearchRadius maxTotalDistance coordinates

partOne :: Int -> Text -> Text
partOne maxSearchRadius =
  pack . show . partOne' maxSearchRadius . map parsePoint . T.lines

partTwo :: Int -> Int -> Text -> Text
partTwo maxSearchRadius maxTotalDistance =
  pack .
    show .
    partTwo' maxSearchRadius maxTotalDistance .
    map parsePoint .
    T.lines
