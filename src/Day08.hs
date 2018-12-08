module Day08 (partOne) where

import Data.Text (Text, pack, unpack)
import Data.Tree (Tree(..))

type Metadata = [Int]

parseInput :: Text -> [Int]
parseInput = map read . words . unpack

buildTree :: [Int] -> Tree Metadata
buildTree = head . fst . buildChildren 1

buildChildren :: Int -> [Int] -> ([Tree Metadata], [Int])
buildChildren 0 rest = ([], rest)
buildChildren _ [] = error "invalid tree input"
buildChildren _ (_:[]) = error "invalid tree input"
buildChildren count (0:metaCount:xs) = (child:otherChildren, rest)
  where
    child = Node
      { rootLabel = take metaCount xs,
        subForest = []
      }
    (otherChildren, rest) = buildChildren (count - 1) (drop metaCount xs)
buildChildren count (childCount:metaCount:xs) = (child:otherChildren, rest)
  where
    child = Node
      { rootLabel = metadata,
        subForest = grandChildren
      }
    (grandChildren, restAfterGrandChildren) =
      buildChildren childCount xs
    metadata = take metaCount restAfterGrandChildren
    restAfterMetadata = drop metaCount restAfterGrandChildren
    (otherChildren, rest) =
      buildChildren (count - 1) restAfterMetadata

partOne' :: [Int] -> Int
partOne' = foldr ((+) . sum) 0 . buildTree

partOne :: Text -> Text
partOne = pack . show . partOne' . parseInput
