import Protolude
import Test.Hspec

import Day1Spec
import Day2Spec
import Day3Spec
import Day4Spec
import Day5Spec

main :: IO ()
main = hspec $ do
  day1
  day2
  day3
  day4
  day5
