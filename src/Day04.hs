module Day04 (partOne) where

import Data.List (foldl', sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Text.Parsec (ParseError, choice, count, many1, parse)
import Text.Parsec.Char (char, digit, space, string)
import Text.Parsec.Text (Parser)

type Month = Int
type Day = Int
type Minute = Int
type Guard = Int

data Date = Date Month Day
  deriving (Eq, Show, Ord)

data Activity
  = BeginsShift Guard
  | FallsAsleep
  | WakesUp
  deriving (Eq, Show)

data Record = Record
  { recordDate :: Date
  , recordMinute :: Minute
  , recordActivity :: Activity
  } deriving (Eq, Show)

instance Ord Record where
  compare
    Record{recordDate = d1, recordMinute = m1}
    Record{recordDate = d2, recordMinute = m2}
    = if d1 /= d2 then compare d1 d2 else compare m1 m2

recordParser :: Parser Record
recordParser = do
  _ <- char '['
  _ <- count 4 digit
  _ <- char '-'
  month <- read <$> count 2 digit
  _ <- char '-'
  day <- read <$> count 2 digit
  _ <- space
  _ <- count 2 digit
  _ <- char ':'
  minute <- read <$> count 2 digit
  _ <- char ']'
  _ <- space
  activity <- activityParser

  return $ Record
    { recordDate = Date month day
    , recordMinute = minute
    , recordActivity = activity
    }

  where
    activityParser :: Parser Activity
    activityParser = choice [beginsShift, fallsAsleep, wakesUp]

    beginsShift :: Parser Activity
    beginsShift = do
      _ <- string "Guard #"
      guard <- read <$> many1 digit
      _ <- string " begins shift"
      return $ BeginsShift guard

    fallsAsleep :: Parser Activity
    fallsAsleep = string "falls asleep" >> return FallsAsleep

    wakesUp :: Parser Activity
    wakesUp = string "wakes up" >> return WakesUp

parseRecord :: Text -> Record
parseRecord input = onLeftError $ parse recordParser "" input
  where
    onLeftError :: Either ParseError Record -> Record
    onLeftError = either (error . showParseError) id

    showParseError :: ParseError -> String
    showParseError err = concat
      [ "Failed to parse record '"
      , unpack input
      , "'\n"
      , show err
      ]

type AsleepCount = Int
type GuardSchedule = Map Minute AsleepCount
type AllSchedules = Map Guard GuardSchedule

buildSchedules :: [Record] -> AllSchedules
buildSchedules records =
  third $ foldl' f (Nothing, Nothing, M.empty) $ sort records

  where
    f :: (Maybe Guard, Maybe Minute, AllSchedules)
      -> Record
      -> (Maybe Guard, Maybe Minute, AllSchedules)
    f (_, _, mp)
      Record{recordActivity = BeginsShift guard}
      = (Just guard, Nothing, mp)
    f (Just guard, _, mp)
      Record{recordActivity = FallsAsleep, recordMinute = sleepStart}
      = (Just guard, Just sleepStart, mp)
    f (Just guard, Just sleepStart, mp)
      Record{recordActivity = WakesUp, recordMinute = sleepEnd}
      = (Just guard, Nothing, trackSleep mp guard sleepStart sleepEnd)
    f state _ = state

    trackSleep :: AllSchedules -> Guard -> Minute -> Minute -> AllSchedules
    trackSleep schedules guard sleepStart sleepEnd =
      M.insertWith
        (\_ s -> updateSchedule s)
        guard
        (updateSchedule M.empty)
        schedules
      where
        updateSchedule :: GuardSchedule -> GuardSchedule
        updateSchedule schedule =
          foldl' trackSleepForMinute schedule [sleepStart..sleepEnd - 1]

        trackSleepForMinute :: GuardSchedule -> Minute -> GuardSchedule
        trackSleepForMinute s m = M.insertWith (\_ x -> x + 1) m 1 s

    third :: (a, b, c) -> c
    third (_, _, c) = c

partOne' :: [Record] -> Int
partOne' records = guardMostAsleep * minuteMostAsleep
  where
    guardMostAsleep :: Guard
    guardMostAsleep =
      fst guardScheduleMostAsleep

    minuteMostAsleep :: Minute
    minuteMostAsleep =
      fst $ head $ sortOn (negate . snd) $
      M.assocs $ snd guardScheduleMostAsleep

    guardScheduleMostAsleep :: (Guard, GuardSchedule)
    guardScheduleMostAsleep =
      head $ sortOn (negate . totalSleep . snd) $
      M.assocs schedules

    totalSleep :: GuardSchedule -> Int
    totalSleep = M.foldl' (+) 0

    schedules :: AllSchedules
    schedules = buildSchedules records

partOne :: Text -> Text
partOne =
  pack . show . partOne' . map parseRecord . T.lines
