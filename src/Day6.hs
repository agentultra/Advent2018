{-# LANGUAGE TemplateHaskell #-}

module Day6 where

import           Control.Lens
import qualified Data.Map.Strict as Map
import           Protolude

data Point =
  Point
  { _x :: Int
  , _y :: Int
  }
  deriving (Eq, Ord, Show)

makeLenses ''Point

-- A Manhattan distance
distance :: Point -> Point -> Int
distance (Point ax ay) (Point bx by) = abs (ax - bx) + abs (ay - by)

distances :: Point -> [Point] -> [(Point, Int)]
distances point = map (\p -> (p, distance point p))

-- Not the smartest way to do this
closestPoint :: Point -> [Point] -> Maybe Point
closestPoint point = onlyOne . take 2 . sortOn snd . distances point
  where
    onlyOne :: [(Point, Int)] -> Maybe Point
    onlyOne [] = Nothing
    onlyOne [x] = Just $ fst x
    onlyOne (x:y:_) =
      if snd x == snd y then Nothing else Just $ fst x

data MapPoint = MapPoint Char Point
  deriving (Eq, Show)

instance Ord MapPoint where
  (MapPoint _ p1) `compare` (MapPoint _ p2) = p1 `compare` p2

areas :: [Point] -> Map Point Char
areas points
  | length points == 0 = Map.empty
  | length points == 1 = Map.empty
  | length points == 2 = Map.empty
  | otherwise          =
    Map.fromList []
