module Day1Spec where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import           Prelude (read)
import           Protolude
import           Test.Hspec

import Day1

readInt :: Text -> Int
readInt = read . dropPlus . toS
  where
    dropPlus str@(c:cs) =
      case c of
        '+' -> cs
        _   -> str

day1 :: Spec
day1 = do
  describe "Part 1" $ do
    it "should sum all integers" $ do
      contents <- fmap Text.lines $ Text.readFile "./test/input/Day1-1.txt"
      let nums = map readInt contents
      part1 nums `shouldBe` 439
