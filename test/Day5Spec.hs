module Day5Spec where

import Protolude
import Test.Hspec

import Day5

day5 :: Spec
day5 = do
  context "Day 5" $ do
    describe "react" $ do
      it "should remove the first Cc pair" $
        part1 "dabAcCaCBAcCcaDA" `shouldBe` 10
