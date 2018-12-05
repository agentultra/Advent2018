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
    let m = M.map sleepIntervals . partitionEvents <$> mapM parseLine (T.lines input)
    case m of
        Right m -> do
            let gid@(GuardId n) = sleepyHead m                                                
            print $ time (maximumBy (compare `on` duration) $ m ! gid) * n -- this number though is the same as guessed last time
            print n 
            print $ time (maximumBy (compare `on` duration) $ m ! gid) * n
        Left err -> print err