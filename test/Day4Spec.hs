module Day4Spec where

import Data.List as L
import Data.Map as M
import Data.Text as T hiding (maximum)
import Data.Text.IO as T hiding (maximum)
import Protolude
import Test.Hspec

import Day4

-- [(GuardId 659,
-- [Row {date = Date 1518 10 3 0 35, event = WakeUp}
-- ,Row {date = Date 1518 9 27 0 30, event = Asleep}
-- ,Row {date = Date 1518 9 25 0 10, event = Asleep}
-- ,Row {date = Date 1518 6 7 0 39, event = WakeUp}
-- ,Row {date = Date 1518 6 3 0 0, event = ShiftStart (GuardId 659)}])
-- ,(GuardId 1699,
-- [Row {date = Date 1518 11 3 0 36, event = Asleep}
-- ,Row {date = Date 1518 10 15 23 59, event = ShiftStart (GuardId 1699)}])])

exampleInput :: Text
exampleInput = T.unlines
    [ "[1518-06-03 00:00] Guard #659 begins shift"
    , "[1518-10-03 00:35] wakes up"
    , "[1518-04-30 00:20] wakes up"
    , "[1518-10-15 23:59] Guard #1699 begins shift"
    , "[1518-06-07 00:39] wakes up"
    , "[1518-09-25 00:10] falls asleep"
    , "[1518-09-27 00:30] falls asleep"
    , "[1518-11-03 00:36] falls asleep"
    ]

day4 :: Spec
day4 = do
  context "Day 4" $ do
    describe "parseLine" $ do
      it "should parse a begins shift row" $ do
        parseLine "[1518-10-15 23:59] Guard #1699 begins shift"
          `shouldBe`
          Right (Row (Date 1518 10 15 23 59) (ShiftStart (GuardId 1699)))

      it "should parse a wakes up row" $ do
        parseLine "[1518-06-07 00:39] wakes up"
          `shouldBe`
          Right (Row (Date 1518 6 7 0 39) WakeUp)

      it "should parse a falls asleep" $ do
        parseLine "[1518-11-03 00:36] falls asleep"
          `shouldBe`
          Right (Row (Date 1518 11 3 0 36) Asleep)

    describe "tagEvent" $ do
      it "tag events and partition them by guard" $ do
        partitionEvents <$> mapM parseLine (T.lines exampleInput)
        `shouldBe`
        Right (M.fromList
                [(GuardId 659,
                   [ Row {date = Date 1518 6 3 0 0, event = ShiftStart (GuardId 659)}
                   , Row {date = Date 1518 6 7 0 39, event = WakeUp}
                   , Row {date = Date 1518 9 25 0 10, event = Asleep}
                   , Row {date = Date 1518 9 27 0 30, event = Asleep}
                   , Row {date = Date 1518 10 3 0 35, event = WakeUp}
                   ])
                ,(GuardId 1699,
                   [ Row {date = Date 1518 10 15 23 59, event = ShiftStart (GuardId 1699)}
                   , Row {date = Date 1518 11 3 0 36, event = Asleep}
                   ])])

    describe "sleepIntervals" $ do

      it "should calculate the sum of sleep intervals partitioned by guard" $ do
        M.map sleepIntervals . partitionEvents <$> mapM parseLine (T.lines exampleInput)
          `shouldBe`
          Right (M.fromList [(GuardId 659,[Interval 24 10 35]),(GuardId 1699,[])])

    describe "sleepyHead" $ do
      it "tweedledee slept the most" $ do
        sleepyHead . M.map sleepIntervals . partitionEvents <$> mapM parseLine (T.lines exampleInput)
        `shouldBe`
        Right (GuardId 659)
      it "tweedledee slept n hours" $ do
        do { m <- M.map sleepIntervals . partitionEvents <$> mapM parseLine (T.lines exampleInput) ;
             pure $ maximum $ L.map duration $ m ! sleepyHead m } `shouldBe` Right 24
