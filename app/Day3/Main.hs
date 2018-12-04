module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Protolude

import Day3

main :: IO ()
main = do
  content <- fmap Text.lines $ Text.readFile "./input/Day3.txt"
  let claims = map parseClaim content
  let part1Solution = part1 claims
  let part2Solution = part2 claims
  putText $ "Part1: " <> (show part1Solution)
  case part2Solution of
    Just solution -> putText $ "Part2: " <> (show solution)
    Nothing       -> putText "Haven't found Part2 solution.. yet"
