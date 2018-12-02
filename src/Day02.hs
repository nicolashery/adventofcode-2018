module Day02 where

import Data.Bool (bool)
import Data.List (group, sort, tails)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

type BoxId = Text

partOne' :: [BoxId] -> Int
partOne' boxes = countTwo boxes * countThree boxes
  where
    countTwo :: [BoxId] -> Int
    countTwo = length . filter hasTwo

    countThree :: [BoxId] -> Int
    countThree = length . filter hasThree

    hasTwo :: BoxId -> Bool
    hasTwo =
      (> 0) . length . filter (\(_, c) -> c == 2) . countOccurences

    hasThree :: BoxId -> Bool
    hasThree =
      (> 0) . length . filter (\(_, c) -> c == 3) . countOccurences

    countOccurences :: BoxId -> [(Char, Int)]
    countOccurences =
      map (\s -> (head s, length s)) . group . sort . unpack

areCharsEqual :: (Char, Char) -> Bool
areCharsEqual (c1, c2) = c1 == c2

distance :: (BoxId, BoxId) -> Int
distance (id1, id2) = sum . map (bool 1 0) . map areCharsEqual $ T.zip id1 id2

areCorrectBoxes :: (BoxId, BoxId) -> Bool
areCorrectBoxes p = distance p == 1

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

commonLetters :: (BoxId, BoxId) -> Text
commonLetters (id1, id2) = pack . map fst . filter areCharsEqual $ T.zip id1 id2

partTwo' :: [BoxId] -> Text
partTwo' = commonLetters . head . filter areCorrectBoxes . pairs

partOne :: Text -> Text
partOne =
  pack . show . partOne' . T.lines

partTwo :: Text -> Text
partTwo =
  partTwo' . T.lines
