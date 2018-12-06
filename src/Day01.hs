module Day01 (partOne, partTwo) where

import Data.List (cycle, foldl')
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Text.Read (readMaybe)

type Frequency = Int
type FrequencyChange = Int

initialFrequency :: Frequency
initialFrequency = 0

parseFrequencyChange :: Text -> FrequencyChange
parseFrequencyChange s =
  case unpack s of
    ('+':xs) -> read' xs
    ('-':xs) -> negate $ read' xs
    _ -> parseError

  where
    read' = fromMaybe parseError . readMaybe

    parseError = error $
      "expected frequency change to be of format '+' or '-'"
      ++ " followed by an integer, got '" ++ unpack s ++ "'"

partOne' :: [FrequencyChange] -> Frequency
partOne' = foldl' (+) initialFrequency

partTwo' :: [FrequencyChange] -> Frequency
partTwo' =
  go S.empty 0 initialFrequency . cycle

  where
    go :: Set Frequency -> Int -> Frequency -> [FrequencyChange] -> Frequency
    go _ _ 1000000 _ = maxIterationsError
    go _ _ _ [] = inputExhaustedError
    go seen iterations frequency (change:futureChanges) =
      if S.member frequency seen
      then frequency
      else go seen' (iterations + 1) frequency' futureChanges
      where
        seen' = S.insert frequency seen
        frequency' = frequency + change

    maxIterationsError = error $
      "could not find a frequency reached twice after"
      ++ " 1000000 iterations"
    inputExhaustedError = error $
      "could not find a frequency reached twice after"
      ++ " exhausting all frequency changes"

partOne :: Text -> Text
partOne =
  pack . show . partOne' . map parseFrequencyChange . T.lines

partTwo :: Text -> Text
partTwo =
  pack . show . partTwo' . map parseFrequencyChange . T.lines
