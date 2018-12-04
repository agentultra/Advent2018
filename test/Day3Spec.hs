module Day3Spec where

import qualified Data.Map as Map
import           Protolude
import           Test.Hspec

import Day3

day3 :: Spec
day3 = do
  context "Day 3" $ do
    describe "Part 1" $ do
      describe "parseClaim" $ do
        it "should parse a formatted claim" $ do
          parseClaim "#1 @ 335,861: 14x10"
            `shouldBe`
            Claim { _claimId = 1
                  , _area = Rectangle { _offsetX = 335
                                      , _offsetY = 861
                                      , _width = 14
                                      , _height = 10
                                      }
                  }

      describe "claimFabric" $ do
        it "should claim example locations" $ do
          let claims =
                map parseClaim
                [ "#1 @ 1,3: 4x4"
                , "#2 @ 3,1: 4x4"
                , "#3 @ 5,5: 2x2"
                ]
              expected =
                Map.fromList
                [ ((1,3),[1])
                , ((1,4),[1])
                , ((1,5),[1])
                , ((1,6),[1])
                , ((2,3),[1])
                , ((2,4),[1])
                , ((2,5),[1])
                , ((2,6),[1])
                , ((3,1),[2])
                , ((3,2),[2])
                , ((3,3),[1,2])
                , ((3,4),[1,2])
                , ((3,5),[1])
                , ((3,6),[1])
                , ((4,1),[2])
                , ((4,2),[2])
                , ((4,3),[1,2])
                , ((4,4),[1,2])
                , ((4,5),[1])
                , ((4,6),[1])
                , ((5,1),[2])
                , ((5,2),[2])
                , ((5,3),[2])
                , ((5,4),[2])
                , ((5,5),[3])
                , ((5,6),[3])
                , ((6,1),[2])
                , ((6,2),[2])
                , ((6,3),[2])
                , ((6,4),[2])
                , ((6,5),[3])
                , ((6,6),[3])
                ]
          claimFabric claims `shouldBe` expected
