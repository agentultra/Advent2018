import Protolude
import Test.Hspec

import Day1Spec
import Day2Spec
import Day3Spec

main :: IO ()
main = hspec $ do
  day1
  day2
  day3
