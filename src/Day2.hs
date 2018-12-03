module Day2 where

import           Data.List hiding (map, sum)
import qualified Data.Text as Text
import           Prelude (String)
import           Protolude

scanID :: Text -> (Int, Int)
scanID id =
  let groups = filter (\x -> x == 2 || x == 3) $ groupLengths $ toS id
      maybe2 = find (== 2) groups
      maybe3 = find (== 3) groups
  in
    (maybe 0 (const 1) maybe2, maybe 0 (const 1) maybe3)
  where
    groupLengths :: String -> [Int]
    groupLengths = map length . group . sort

part1 :: [Text] -> Int
part1 ids =
  let sumCounts = sumGroups $ map scanID ids
  in
    uncurry (*) sumCounts
  where
    sumGroups :: [(Int, Int)] -> (Int, Int)
    sumGroups = foldl' (\(l, r) (l', r') -> (l + l', r + r')) (0, 0)

part2 :: [Text] -> Text
part2 ids =
  let cs = map (\(x, y) -> (toS x, toS y)) $ combinations ids
      r  = [ x `intersect` y | (x, y) <- cs, isDiffByOne x y]
  in
    Text.concat $ map Text.pack r

combinations :: [a] -> [(a, a)]
combinations xs = [(x, y) | (x:xs') <- tails xs, y <- xs']

isDiffByOne :: Eq a => [a] -> [a] -> Bool
isDiffByOne xs ys
  | xs == ys  = False
  | otherwise =
    1 == (sum $ zipWith (\x y -> if x == y then 0 else 1) xs ys)
