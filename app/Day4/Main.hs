module Main where

import Data.List as L
import Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Protolude

import Day4

main :: IO ()
main = do
  let fileName = "./input/Day04.txt"
  input <- T.readFile fileName
  let partitionedEvents = partitionEvents <$> mapM parseLine (T.lines input)
  let m = M.map sleepIntervals <$> partitionedEvents
  case m of
    Right m -> do
      let gid@(GuardId n) = sleepyHead m
      let foo = maximumBy (compare `on` snd) $ M.toList $ asleepMinutes $ m ! gid
      print (((fst foo)) * n)
    Left err -> print err
