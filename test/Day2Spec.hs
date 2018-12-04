module Day2Spec where

import Protolude
import Test.Hspec

import Day2
import Lib

day2 :: Spec
day2 = do
  context "Day 2" $ do
    describe "Part 1" $ do
      it "should count no repeating letters" $ do
        scanID "abcdef" `shouldBe` (0, 0)

      it "should count if it contains groups of 2 and 3" $ do
        scanID "bababc" `shouldBe` (1, 1)

      it "should count groups of 2 in first position" $ do
        scanID "abbcde" `shouldBe` (1, 0)

      it "should count groups of 3 in second position" $ do
        scanID "abcccd" `shouldBe` (0, 1)

      it "should count groups of 2 only once" $ do
        scanID "aabcdd" `shouldBe` (1, 0)

      it "should count groups of 3 only once" $ do
        scanID "ababab" `shouldBe` (0, 1)

      it "should checksum ids with no groups as 0" $ do
        part1 ["abc", "def"] `shouldBe` 0

      it "should checksum as 0 with only groups of 2" $ do
        part1 ["aabc", "aabc"] `shouldBe` 0

      it "should checksum as 1 with one group of each" $ do
        part1 ["aabc", "aaabc"] `shouldBe` 1

    describe "Part 2" $ do
      context "combinations" $ do
        it "should generate a single pair for a 2 element list" $ do
          combinations ["a", "b"] `shouldBe` [("a", "b")]

        it "should generate all combinations" $ do
          combinations ["a", "b", "c"]
            `shouldBe`
            [ ("a", "b")
            , ("a", "c")
            , ("b", "c")
            ]

      context "isDiffByOne" $ do
        it "should return False if lists are same" $ do
          isDiffByOne "abc" "abc" `shouldBe` False

        it "should return False if lists differ by more than one" $ do
          isDiffByOne "abcde" "axcye" `shouldBe` False

        it "should return True if lists differ by exactly one" $ do
          isDiffByOne "fghij" "fguij" `shouldBe` True

      context "solution" $ do
        it "should return the common characters of the neighbours" $ do
          part2 ["fghij", "fguij"] `shouldBe` "fgij"

        it "should return the common characters of all the neighbours" $ do
          part2 ["fghij", "fguij", "abcde", "abcdf"] `shouldBe` "fgijabcd"
