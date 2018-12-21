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
        distances (Point 0 0)
        [ MapPoint 'a' (Point 0 1)
        , MapPoint 'b' (Point 5 8)
        , MapPoint 'c' (Point (-5) (-8))
        ]
        `shouldBe`
        [ (MapPoint 'a' (Point 0 1), 1)
        , (MapPoint 'b' (Point 5 8), 13)
        , (MapPoint 'c' (Point (-5) (-8)), 13)
        ]

    describe "closestMapPoint" $ do
      it "should return Nothing for equidistant points" $
        closestMapPoint (Point 0 0) [MapPoint 'a' (Point 0 1), MapPoint 'b' (Point 0 (-1))]
        `shouldBe`
        Nothing

      it "should return Just the one point" $
        closestMapPoint (Point 0 0) [MapPoint 'a' (Point 0 1)]
        `shouldBe`
        Just (MapPoint 'a' (Point 0 1))

      it "should return the closest of many points" $
        closestMapPoint (Point 0 0)
        [ MapPoint 'a' (Point 12 23)
        , MapPoint 'b' (Point 2 2)
        , MapPoint 'c' (Point 3 3)]
        `shouldBe`
        Just (MapPoint 'b' (Point 2 2))

    describe "areas" $ do
      it "should return the empty map for empty list of points" $
        areas [] `shouldBe` Nothing

      it "should return empty map for 1 point" $
        areas [Point 0 0] `shouldBe` Nothing

      it "should return empty map for 2 points" $
        areas [Point 0 0, Point 2 2] `shouldBe` Nothing

      it "should return a map for 3 points" $
        areas [Point 0 0, Point 3 3, Point 9 9]
        `shouldBe`
        (Just $ MapArea (Point 0 0) (Point 9 9) $
         Map.fromList
         [ (Point {_x = 0, _y = 0}, 'a')
         , (Point {_x = 0, _y = 3}, '.')
         , (Point {_x = 0, _y = 4}, '.')
         , (Point {_x = 0, _y = 5}, '.')
         , (Point {_x = 0, _y = 6}, '.')
         , (Point {_x = 0, _y = 7}, '.')
         , (Point {_x = 0, _y = 8}, '.')
         , (Point {_x = 0, _y = 9}, '.')
         , (Point {_x = 1, _y = 2}, '.')
         , (Point {_x = 1, _y = 9}, '.')
         , (Point {_x = 2, _y = 1}, '.')
         , (Point {_x = 2, _y = 9}, '.')
         , (Point {_x = 3, _y = 0}, '.')
         , (Point {_x = 3, _y = 3}, 'b')
         , (Point {_x = 3, _y = 9}, '.')
         , (Point {_x = 4, _y = 0}, '.')
         , (Point {_x = 4, _y = 8}, '.')
         , (Point {_x = 5, _y = 0}, '.')
         , (Point {_x = 5, _y = 7}, '.')
         , (Point {_x = 6, _y = 0}, '.')
         , (Point {_x = 6, _y = 6}, '.')
         , (Point {_x = 7, _y = 0}, '.')
         , (Point {_x = 7, _y = 5}, '.')
         , (Point {_x = 8, _y = 0}, '.')
         , (Point {_x = 8, _y = 4}, '.')
         , (Point {_x = 9, _y = 0}, '.')
         , (Point {_x = 9, _y = 1}, '.')
         , (Point {_x = 9, _y = 2}, '.')
         , (Point {_x = 9, _y = 3}, '.')
         , (Point {_x = 9, _y = 9}, 'c')
         ])

    describe "largestFiniteArea" $ do
      it "should return Nothing if none can be found" $
        largestFiniteArea [Point 0 0, Point 9 9]
        `shouldBe`
        Nothing

      it "should return 1 for a small finite area" $
        largestFiniteArea [Point 0 0, Point 3 3, Point 9 9]
        `shouldBe`
        Just 1
