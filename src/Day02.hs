module Day02 where

import Data.List (group, sort)
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

    countOccurences :: BoxId -> [(Char, Int)]
    countOccurences =
      map (\s -> (head s, length s)) . group . sort . unpack

    hasTwo :: BoxId -> Bool
    hasTwo =
      (> 0) . length . filter (\(_, c) -> c == 2) . countOccurences

    hasThree :: BoxId -> Bool
    hasThree =
      (> 0) . length . filter (\(_, c) -> c == 3) . countOccurences

partOne :: Text -> Text
partOne =
  pack . show . partOne' . T.lines
