module Day4 where

import Data.Time hiding (Day)
import Data.Map as M
import Data.List as L
import Prelude (read)
import Protolude hiding ((<|>), many)
import Text.Parsec hiding (State)

type Year = Int
type Month = Int
type Day = Int
type Hour = Int
type Minute = Int

newtype GuardId = GuardId Int
    deriving (Eq,Ord,Show)

data Date = Date Year Month Day Hour Minute
    deriving (Eq,Ord,Show)

data Event = ShiftStart GuardId | Asleep | WakeUp
    deriving (Eq,Ord,Show)

data Row =
  Row
  { date :: UTCTime
  , event :: Event
  }
  deriving (Eq,Ord,Show)

    -- , "[1518-10-03 00:35] wakes up"
    -- , "[1518-04-30 00:20] wakes up"
    -- , "[1518-10-15 23:59] Guard #1699 begins shift"
    -- , "[1518-06-07 00:39] wakes up"
    -- , "[1518-09-25 00:10] falls asleep"
    -- , "[1518-09-27 00:30] falls asleep"
    -- , "[1518-11-03 00:36] falls asleep"

parseEvent :: Parsec Text () Event
parseEvent =
  (Asleep <$ string "falls asleep") <|>
  (WakeUp <$ string "wakes up") <|>
  (string "Guard #" *> (ShiftStart . GuardId <$> parseInt Nothing) <* string " begins shift")

parseInt :: Maybe Int -> Parsec Text () Int
parseInt = fmap read . maybe (many digit) (`count` digit)

mkDate :: Year -> Month -> Day -> Hour -> Minute -> UTCTime
mkDate yy mm dd hh min =
  UTCTime (fromGregorian (toInteger yy) mm dd)
  (secondsToDiffTime $ toInteger $ (60 * hh + min) * 60)


lineParser :: Parsec Text () Row
lineParser = do
    char '['
    yy <- parseInt (Just 4) <* char '-'
    mm <- parseInt (Just 2) <* char '-'
    dd <- parseInt (Just 2) <* char ' '
    hh <- parseInt (Just 2) <* char ':'
    min <- parseInt (Just 2)
    char ']' >> char ' '
    let d = mkDate yy mm dd hh min
    Row d <$> parseEvent

parseLine :: Text -> Either ParseError Row
parseLine = parse lineParser "my-file.ext"

partitionEvents :: [Row] -> Map GuardId [Row]
partitionEvents = M.map L.reverse . M.fromListWith (++) . L.map (second pure) . tagEvent

tagEvent :: [Row] -> [(GuardId,Row)]
tagEvent xs = catMaybes $ evalState (mapM tag $ L.sortOn date xs) (Nothing :: Maybe GuardId)
  where
    tag r@(Row d (ShiftStart gid)) = put (Just gid) >> pure (Just (gid,r))
    tag r = do
      gid <- get
      pure $ (,) <$> gid <*> pure r

data Interval =
  Interval
  { duration :: NominalDiffTime
  , time :: Minute
  }
  deriving (Eq, Show)

sleepIntervals :: [Row] -> [Interval]
sleepIntervals xs = catMaybes $ evalState (mapM tag xs) (Nothing :: Maybe UTCTime)
  where
    tag :: Row -> State (Maybe UTCTime) (Maybe Interval)
    tag (Row d Asleep) = do
      s <- get
      maybe (put $ Just d) (const $ pure ()) s
      pure Nothing
    tag (Row d WakeUp) = do
      s <- get
      case s of
        Nothing -> pure Nothing
        Just d' -> do
          put Nothing
          pure $ Just $ Interval ((d `diffUTCTime` d') - 60) (((floor (toRational $ utctDayTime d) `div` 60) `mod` 60) - 1)
    tag _doh = pure Nothing

sleepyHead :: Map GuardId [Interval] -> GuardId
sleepyHead = fst . L.maximumBy (compare `on` snd) . M.toList . M.map (L.sum . L.map duration)
