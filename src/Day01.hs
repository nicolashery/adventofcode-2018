module Day01 where

import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Text.Read (readMaybe)

type Frequency = Int

data FrequencyChange
  = Inc Int
  | Dec Int
  deriving (Eq, Show)

parseFrequencyChange :: Text -> FrequencyChange
parseFrequencyChange s =
  case unpack s of
    ('+':xs) -> Inc (read' xs)
    ('-':xs) -> Dec (read' xs)
    _ -> parseError

  where
    read' = fromMaybe parseError . readMaybe

    parseError = error $
      "expected frequency change to be of format '+' or '-'"
      ++ " followed by an integer, got '" ++ unpack s ++ "'"

applyFrequencyChange :: Frequency -> FrequencyChange -> Frequency
applyFrequencyChange x (Inc dx) = x + dx
applyFrequencyChange x (Dec dx) = x - dx

solve :: Text -> Text
solve input =
  pack . show $ foldl f 0 $ T.lines input

  where
    f :: Frequency -> Text -> Frequency
    f x s = applyFrequencyChange x (parseFrequencyChange s)
