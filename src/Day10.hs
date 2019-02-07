module Day10 (partOne) where

import Data.Bool (bool)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import Data.Text.IO (readFile)
import Text.Parsec
  (ParseError, many1, optionMaybe, optional, parse, skipMany, try, (<|>))
import Text.Parsec.Char (char, digit, space, string)
import Text.Parsec.Text (Parser)

data Point = Point
  { pointPosition :: (Int, Int)
  , pointVelocity :: (Int, Int)
  } deriving (Eq, Show)

-- position=< 6, 10> velocity=<-2, -1>
pointParser :: Parser Point
pointParser = do
  _ <- string "position="
  position <- pair
  _ <- string " velocity="
  velocity <- pair
  return $ Point position velocity

  where
    pair :: Parser (Int, Int)
    pair = do
      _ <- char '<'
      x <- paddedInt
      _ <- char ','
      y <- paddedInt
      _ <- char '>'
      return (x, y)

    paddedInt :: Parser Int
    paddedInt = do
      skipMany space
      isNegative <- negative
      n <- int
      return $ bool id negate isNegative $ n

    negative :: Parser Bool
    negative =
      isJust <$> (optionMaybe . try . char $ '-')

    int :: Parser Int
    int = read <$> many1 digit

parsePoint :: Text -> Point
parsePoint input = onLeftError $ parse pointParser "" input
  where
    onLeftError :: Either ParseError Point -> Point
    onLeftError = either (error . showParseError) id

    showParseError :: ParseError -> String
    showParseError err = concat
      [ "Failed to parse point '"
      , unpack input
      , "'\n"
      , show err
      ]

movePoint :: Point -> Point
movePoint p =
  let (x, y) = pointPosition p
      (vx, vy) = pointVelocity p
  in p { pointPosition = (x + vx, y + vy) }

numberPointsAlignedTopAndBottom :: [Point] -> Int
numberPointsAlignedTopAndBottom ps = nYmin + nYmax
  where
    nYmin :: Int
    nYmin = length . filter (\p -> pointY p == yMin) $ ps

    nYmax :: Int
    nYmax = length . filter (\p -> pointY p == yMax) $ ps

    pointY :: Point -> Int
    pointY (Point (_, y) _) = y

    yMin :: Int
    yMin = minimum . map pointY $ ps

    yMax :: Int
    yMax = maximum . map pointY $ ps

step :: ([Point], Int) -> ([Point], Int)
step (ps, _) = (ps', numberPointsAlignedTopAndBottom ps')
  where
    ps' = map movePoint ps

-- map snd $ take 10 $ iterate' step (puzzleInput, numberPointsAlignedTopAndBottom puzzleInput)

drawPoints :: [Point] -> Text
drawPoints ps = undefined -- TODO: next
  where
    grid :: [[(Int,Int)]]
    grid = [[(x, y) | x <- [0..xMax - 1]] | y <- [0..yMax - 1]]

puzzleInput :: [Point]
puzzleInput =
  map parsePoint $
    [ "position=< 9,  1> velocity=< 0,  2>"
    , "position=< 7,  0> velocity=<-1,  0>"
    , "position=< 3, -2> velocity=<-1,  1>"
    , "position=< 6, 10> velocity=<-2, -1>"
    , "position=< 2, -4> velocity=< 2,  2>"
    , "position=<-6, 10> velocity=< 2, -2>"
    , "position=< 1,  8> velocity=< 1, -1>"
    , "position=< 1,  7> velocity=< 1,  0>"
    , "position=<-3, 11> velocity=< 1, -2>"
    , "position=< 7,  6> velocity=<-1, -1>"
    , "position=<-2,  3> velocity=< 1,  0>"
    , "position=<-4,  3> velocity=< 2,  0>"
    , "position=<10, -3> velocity=<-1,  1>"
    , "position=< 5, 11> velocity=< 1, -2>"
    , "position=< 4,  7> velocity=< 0, -1>"
    , "position=< 8, -2> velocity=< 0,  1>"
    , "position=<15,  0> velocity=<-2,  0>"
    , "position=< 1,  6> velocity=< 1,  0>"
    , "position=< 8,  9> velocity=< 0, -1>"
    , "position=< 3,  3> velocity=<-1,  1>"
    , "position=< 0,  5> velocity=< 0, -1>"
    , "position=<-2,  2> velocity=< 2,  0>"
    , "position=< 5, -2> velocity=< 1,  2>"
    , "position=< 1,  4> velocity=< 2,  1>"
    , "position=<-2,  7> velocity=< 2, -2>"
    , "position=< 3,  6> velocity=<-1, -1>"
    , "position=< 5,  0> velocity=< 1,  0>"
    , "position=<-6,  0> velocity=< 2,  0>"
    , "position=< 5,  9> velocity=< 1, -2>"
    , "position=<14,  7> velocity=<-2,  0>"
    , "position=<-3,  6> velocity=< 2, -1>"
    ]

partOne :: Text -> Text
partOne = undefined
