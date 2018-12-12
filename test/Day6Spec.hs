module Day6Spec where

import qualified Data.Map.Strict as Map
import Protolude
import Test.Hspec

import Day6

day6 :: Spec
day6 =
  context "Day 6" $ do
    describe "distance" $ do
      it "should calculate the distance between two points 1 space apart" $
        distance (Point 0 0) (Point 0 1) `shouldBe` 1

      it "should calculate distances further apart" $
        distance (Point 0 0) (Point 5 8) `shouldBe` 13

      it "should handle points with negative components" $
        distance (Point 0 0) (Point (-5) (-8)) `shouldBe` 13

    describe "distances" $ do
      it "should return the empty list if given no points to compare" $
        distances (Point 0 0) [] `shouldBe` []

      it "should return a list of distances" $
        distances (Point 0 0) [Point 0 1, Point 5 8, Point (-5) (-8)]
        `shouldBe`
        [(Point 0 1, 1), (Point 5 8, 13), (Point (-5) (-8), 13)]

    describe "closestPoint" $ do
      it "should return Nothing for equidistant points" $
        closestPoint (Point 0 0) [Point 0 1, Point 0 (-1)]
        `shouldBe`
        Nothing

      it "should return Just the one point" $
        closestPoint (Point 0 0) [Point 0 1] `shouldBe` Just (Point 0 1)

      it "should return the closest of many points" $
        closestPoint (Point 0 0) [Point 12 23, Point 2 2, Point 3 3]
        `shouldBe`
        Just (Point 2 2)

    describe "areas" $ do
      it "should return the empty map for empty list of points" $
        areas [] `shouldBe` Map.empty

      it "should return empty map for 1 point" $
        areas [Point 0 0] `shouldBe` Map.empty

      it "should return empty map for 2 points" $
        areas [Point 0 0, Point 2 2] `shouldBe` Map.empty

      it "should return a map for 3 points" $
        areas [Point 0 0, Point 3 3, Point 9 9]
        `shouldBe`
        Map.empty
