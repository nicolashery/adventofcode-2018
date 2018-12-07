module Day05 (partOne) where

import Data.Char (isLower, isUpper, toLower)
import Data.Text (Text, pack, unpack)

type PolymerUnit = Char
type Polymer = [PolymerUnit]

canReact :: PolymerUnit -> PolymerUnit -> Bool
canReact x y
  | isLower x && isUpper y && x == toLower y = True
  | isUpper x && isLower y && toLower x == y = True
  | otherwise = False

reduce :: Polymer -> Polymer
reduce original = go [] original
  where
    go :: Polymer -> Polymer -> Polymer
    go reversedLeft [] = reverse reversedLeft
    go reversedLeft (y:[]) =
      -- Input from file may contain trailing newline
      if y == '\n'
      then reverse reversedLeft
      else reverse (y:reversedLeft)
    go reversedLeft (x:y:right) =
      if canReact x y
      then
      go [] (reverse reversedLeft ++ right)
      else
      go (x:reversedLeft) (y:right)

partOne' :: Polymer -> Int
partOne' = length . reduce

partOne :: Text -> Text
partOne = pack . show . partOne' . unpack
