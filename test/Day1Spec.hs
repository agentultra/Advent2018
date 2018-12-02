module Day1Spec where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import           Prelude (read)
import           Protolude
import           Test.Hspec

import Day1

day1 :: Spec
day1 = do
  describe "Part 1" $ do
    it "should sum to -13" $ do
      part1 [(-2), 3, 4, (-18)] `shouldBe` (-13)
    it "should sum to 0" $ do
      part1 [1, (-1)] `shouldBe` 0

  describe "Part 2" $ do
    it "should find 0" $ do
      part2 [1, (-1)] `shouldBe` 0
    it "should find 10" $ do
      part2 [3, 3, 4, (-2), (-4)] `shouldBe` 10
    it "should find 5" $ do
      part2 [(-6), 3, 8, 5, (-6)] `shouldBe` 5
    it "should find 14" $ do
      part2 [7, 7, (-2), (-7), (-4)] `shouldBe` 14
