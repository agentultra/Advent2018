module Day2 where

import Prelude (String)
import Protolude

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
    (fst sumCounts) * (snd sumCounts)
  where
    sumGroups :: [(Int, Int)] -> (Int, Int)
    sumGroups = foldl' (\(l, r) (l', r') -> (l + l', r + r')) (0, 0)
