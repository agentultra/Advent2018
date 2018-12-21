{-# LANGUAGE TemplateHaskell #-}

module Day6 where

import           Control.Lens
import           Data.List hiding (map, sortOn)
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

distances :: Point -> [MapPoint] -> [(MapPoint, Int)]
distances point = map (\p@(MapPoint _ p') -> (p, distance point p'))

-- Not the smartest way to do this
closestMapPoint :: Point -> [MapPoint] -> Maybe MapPoint
closestMapPoint point = onlyOne . take 2 . sortOn snd . distances point
  where
    onlyOne :: [(MapPoint, Int)] -> Maybe MapPoint
    onlyOne [] = Nothing
    onlyOne [x] = Just $ fst x
    onlyOne (x:y:_) =
      if snd x == snd y then Nothing else Just $ fst x

data MapPoint =
  MapPoint
  { _ch         :: Char
  , _point      :: Point
  }
  deriving (Eq, Show)

makeLenses ''MapPoint

data MapArea =
  MapArea
  { _topLeft     :: Point
  , _bottomRight :: Point
  , _areaMap     :: Map Point Char
  }
  deriving (Eq, Show)

makeLenses ''MapArea

instance Ord MapPoint where
  (MapPoint _ p1) `compare` (MapPoint _ p2) = p1 `compare` p2

areas :: [Point] -> Maybe MapArea
areas points
  | length points == 0 = Nothing
  | length points == 1 = Nothing
  | length points == 2 = Nothing
  | otherwise          =
    let mapPoints = zipWith (\c p -> MapPoint c p) ['a'..] points
        minPoint  = minimumBy (\(MapPoint _ p1) (MapPoint _ p2) ->
                                 p1 `compare` p2) mapPoints
        maxPoint  = maximumBy (\(MapPoint _ p1) (MapPoint _ p2) ->
                                 p1 `compare` p2) mapPoints
    in
      Just $ MapArea
      (minPoint^.point)
      (maxPoint^.point)
      $ Map.fromList [ getClosestPoint mapPoints (Point x y) |
                       x <- [(minPoint^.point^.x)..(maxPoint^.point^.x)],
                       y <- [(minPoint^.point^.y)..(maxPoint^.point^.y)]]
      where
        getClosestPoint :: [MapPoint] -> Point -> (Point, Char)
        getClosestPoint points point =
          case closestMapPoint point points of
            Just (MapPoint c p) -> (p, c)
            Nothing -> (point, '.')

largestFiniteArea :: [Point] -> Maybe Int
largestFiniteArea = fmap maybeFindAreaSize . areas
  where
    maybeFindAreaSize :: MapArea -> Int
    maybeFindAreaSize (MapArea tl br areaMap) =
      maximum .
      map length .
      groupBy (\(_, c1) (_, c2) -> c1 == c2) .
      filter (edgePoint tl br) .
      filter (\(_, c) -> c /= '.') $ Map.toList areaMap
    edgePoint tl br (p, _) =
      p^.x == tl^.x ||
      p^.y == tl^.y ||
      p^.x == br^.x ||
      p^.y == br^.y
