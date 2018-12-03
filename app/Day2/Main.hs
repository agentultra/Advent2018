module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Protolude

import Day2

main :: IO ()
main = do
  contents <- fmap Text.lines $ Text.readFile "./input/Day2.txt"
  let part1Solution = part1 contents
  let part2Solution = part2 contents
  putText $ "Part1: " <> (show part1Solution)
  putText $ "Part2: " <> part2Solution
