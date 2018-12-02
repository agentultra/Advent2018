{-# LANGUAGE TemplateHaskell #-}

module Day1 where

import           Control.Lens
import           Data.Set (Set)
import qualified Data.Set as Set
import           Protolude

data FrequencyModulatorState =
  FrequencyModulatorState
  { _currentFrequency :: Int
  , _seenFrequencies :: Set Int
  , _deltas :: [Int]
  }

makeLenses ''FrequencyModulatorState

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 nums = evalState
  (myProg 0)
  (FrequencyModulatorState 0 (Set.fromList [0]) nums)
  where
    myProg :: Int -> State FrequencyModulatorState Int
    myProg idx = do
      f  <- use currentFrequency
      fs <- use seenFrequencies
      ds <- use deltas
      case ds ^? ix idx of
        Just x -> do
          let f' = f + x
          case Set.member f' fs of
            True -> return f'
            False -> do
              currentFrequency .= f'
              seenFrequencies .= Set.insert f' fs
              myProg $ idx + 1
        Nothing -> myProg 0
