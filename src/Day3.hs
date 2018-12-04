module Day3 where

import           Data.List ((\\), head, nub, sort)
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map as Map
import           Prelude ((!!), read)
import           Protolude hiding (head)

import Lib

type ClaimID = Int
type Inch = Int
type Position = (Int, Int)
type Fabric = Map Position [ClaimID]

data Rectangle =
  Rectangle
  { _offsetX :: Inch
  , _offsetY :: Inch
  , _width   :: Inch
  , _height  :: Inch
  }
  deriving (Eq, Ord, Show)

data Claim =
  Claim
  { _claimId :: ClaimID
  , _area    :: Rectangle
  }
  deriving (Eq, Ord, Show)

parseClaim :: Text -> Claim
parseClaim input = Claim id (Rectangle x y w h)
  where
    tokens = splitOneOf "#@,:x " $ toS input
    id = read $ tokens !! 1
    x = read $ tokens !! 4
    y = read $ tokens !! 5
    w = read $ tokens !! 7
    h = read $ tokens !! 8

claimFabric :: [Claim] -> Fabric
claimFabric  = foldl stakeClaim Map.empty
  where
    stakeClaim fabric (Claim id (Rectangle x y w h)) =
      foldl claimInch fabric positions
      where
        positions = [(i, j) | i <- [x..x + w - 1], j <- [y..y + h - 1]]
        claimInch f pos = Map.insert pos (claimIds <> [id]) f
          where
            claimIds = Map.findWithDefault [] pos f

part1 :: [Claim] -> Int
part1 claims =
  length $ filter (> 1) $ map length $ Map.elems claimedFabric
  where
    claimedFabric = claimFabric claims

part2 :: [Claim] -> Maybe Int
part2 claims =
  findNonOverlapping claimIds
  where
    findNonOverlapping [] = Nothing
    findNonOverlapping (ids:rest)
      | length ids == 1 && idIsAlone = Just $ head ids
      | otherwise = findNonOverlapping rest
      where
        idIsAlone = not $ any identity $ map (elem (head ids)) ((\\) claimIds [ids])
    claimIds = nub $ sort $ Map.elems fabric
    fabric = claimFabric claims
