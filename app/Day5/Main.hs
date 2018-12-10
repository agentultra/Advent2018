module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Protolude

import Day5

main :: IO ()
main = do
  content <- Text.readFile "./input/Day5.txt"
  let part1Solution = part1 $ toS content
  let part2Solution = part2 $ toS content
  putText $ "Part 1: " <> (show part1Solution)
  putText $ "Part 2: " <> (show part2Solution)
