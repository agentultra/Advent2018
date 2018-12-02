module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Protolude

import Day1
import Lib

main :: IO ()
main = do
  contents <- fmap Text.lines $ Text.readFile "./input/Day1.txt"
  let nums = map readInt contents
  let part1Solution = part1 nums
  let part2Solution = part2 nums
  putText $ "Part1: " <> (show part1Solution)
  putText $ "Part2: " <> (show part2Solution)
