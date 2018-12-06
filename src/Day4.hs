{-# LANGUAGE TemplateHaskell #-}

module Day4 where

import Control.Lens
import Data.Time hiding (Day)
import qualified Data.Map as M
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

data Date =
  Date
  { _year :: Year
  , _month :: Month
  , _day :: Day
  , _hour :: Hour
  , _minute :: Minute
  }
  deriving (Eq,Ord,Show)

makeLenses ''Date

data Event = ShiftStart GuardId | Asleep | WakeUp
    deriving (Eq,Ord,Show)

data Row =
  Row
  { date :: Date
  , event :: Event
  }
  deriving (Eq,Ord,Show)

parseEvent :: Parsec Text () Event
parseEvent =
  (Asleep <$ string "falls asleep") <|>
  (WakeUp <$ string "wakes up") <|>
  (string "Guard #" *> (ShiftStart . GuardId <$> parseInt Nothing) <* string " begins shift")

parseInt :: Maybe Int -> Parsec Text () Int
parseInt = fmap read . maybe (many digit) (`count` digit)

lineParser :: Parsec Text () Row
lineParser = do
    char '['
    yy <- parseInt (Just 4) <* char '-'
    mm <- parseInt (Just 2) <* char '-'
    dd <- parseInt (Just 2) <* char ' '
    hh <- parseInt (Just 2) <* char ':'
    min <- parseInt (Just 2)
    char ']' >> char ' '
    let d = Date yy mm dd hh min
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
  { duration :: Minute
  , start :: Minute
  , end :: Minute
  }
  deriving (Eq, Show)

sleepIntervals :: [Row] -> [Interval]
sleepIntervals xs = catMaybes $ evalState (mapM tag xs) (Nothing :: Maybe Date)
  where
    tag :: Row -> State (Maybe Date) (Maybe Interval)
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
          pure $ Just $ Interval ((d^.minute - 1) - d'^.minute) (d'^.minute) (d^.minute)
    tag _ = pure Nothing

sleepyHead :: Map GuardId [Interval] -> GuardId
sleepyHead = fst . L.maximumBy (compare `on` snd) . M.toList . M.map (L.sum . L.map duration)

asleepMinutes :: [Interval] -> Map Minute Int
asleepMinutes = foldl' countMins (M.fromList [(x, 0) | x <- [0..59]])
  where
    countMins :: Map Minute Int -> Interval -> Map Minute Int
    countMins acc (Interval _ start end) =
      M.mapWithKey (\m c -> if start <= m && m <= end then c + 1 else c) acc
