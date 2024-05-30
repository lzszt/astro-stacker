{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Align where

import Control.Applicative
import Control.Arrow
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable
import Data.Function
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ord
import Debug.Trace
import Matching
import Numeric.LinearAlgebra qualified as LA
import Types

data TransformationType
  = TT_LINEAR -- 0,
  | TT_BILINEAR -- 1,
  | TT_BISQUARED -- 2,
  | TT_BICUBIC -- 3,
  | TT_NONE -- 4,
  | TT_LAST -- 5
  deriving (Show, Eq)

data BilinearParameters = BilinearParameters
  { tType :: TransformationType,
    a0 :: Double,
    a1 :: Double,
    a2 :: Double,
    a3 :: Double,
    a4 :: Double,
    a5 :: Double,
    a6 :: Double,
    a7 :: Double,
    a8 :: Double,
    a9 :: Double,
    a10 :: Double,
    a11 :: Double,
    a12 :: Double,
    a13 :: Double,
    a14 :: Double,
    a15 :: Double,
    b0 :: Double,
    b1 :: Double,
    b2 :: Double,
    b3 :: Double,
    b4 :: Double,
    b5 :: Double,
    b6 :: Double,
    b7 :: Double,
    b8 :: Double,
    b9 :: Double,
    b10 :: Double,
    b11 :: Double,
    b12 :: Double,
    b13 :: Double,
    b14 :: Double,
    b15 :: Double,
    xWidth :: Double,
    yWidth :: Double
  }
  deriving (Show)

transform :: BilinearParameters -> Position -> Position
transform bp pt
  | bp.tType == TT_BICUBIC =
      let x = fromIntegral pt.x / bp.xWidth
          y = fromIntegral pt.y / bp.yWidth
          x2 = x * x
          y2 = y * y
          x3 = x * x * x
          y3 = y * y * y
       in Position
            { x =
                round $
                  ( bp.a0
                      + bp.a1 * x
                      + bp.a2 * y
                      + bp.a3 * x * y
                      + bp.a4 * x2
                      + bp.a5 * y2
                      + bp.a6 * x2 * y
                      + bp.a7 * x * y2
                      + bp.a8 * x2 * y2
                      + bp.a9 * x3
                      + bp.a10 * y3
                      + bp.a11 * x3 * y
                      + bp.a12 * x * y3
                      + bp.a13 * x3 * y2
                      + bp.a14 * x2 * y3
                      + bp.a15 * x3 * y3
                  )
                    * bp.xWidth,
              y =
                round $
                  ( bp.b0
                      + bp.b1 * x
                      + bp.b2 * y
                      + bp.b3 * x * y
                      + bp.b4 * x2
                      + bp.b5 * y2
                      + bp.b6 * x2 * y
                      + bp.b7 * x * y2
                      + bp.b8 * x2 * y2
                      + bp.b9 * x3
                      + bp.b10 * y3
                      + bp.b11 * x3 * y
                      + bp.b12 * x * y3
                      + bp.b13 * x3 * y2
                      + bp.b14 * x2 * y3
                      + bp.b15 * x3 * y3
                  )
                    * bp.yWidth
            }
  | bp.tType == TT_BISQUARED =
      let x = fromIntegral pt.x / bp.xWidth
          y = fromIntegral pt.y / bp.yWidth
          x2 = x * x
          y2 = y * y
       in Position
            { x =
                round $
                  ( bp.a0
                      + bp.a1 * x
                      + bp.a2 * y
                      + bp.a3 * x * y
                      + bp.a4 * x2
                      + bp.a5 * y2
                      + bp.a6 * x2 * y
                      + bp.a7 * x * y2
                      + bp.a8 * x2 * y2
                  )
                    * bp.xWidth,
              y =
                round $
                  ( bp.b0
                      + bp.b1 * x
                      + bp.b2 * y
                      + bp.b3 * x * y
                      + bp.b4 * x2
                      + bp.b5 * y2
                      + bp.b6 * x2 * y
                      + bp.b7 * x * y2
                      + bp.b8 * x2 * y2
                  )
                    * bp.yWidth
            }
  | otherwise =
      let x = fromIntegral pt.x / bp.xWidth
          y = fromIntegral pt.y / bp.yWidth
       in Position
            { -- FIXME (felix): Should this be rounding here?
              x = round $ (bp.a0 + bp.a1 * x + bp.a2 * y + bp.a3 * x * y) * bp.xWidth,
              y = round $ (bp.b0 + bp.b1 * x + bp.b2 * y + bp.b3 * x * y) * bp.yWidth
            }

validateTransformation :: BilinearParameters -> [(Star, Star)] -> Double
validateTransformation bp votingPairs =
  maximum $
    map
      (\(refStar, tgtStar) -> distance refStar.starPosition (transform bp tgtStar.starPosition))
      votingPairs

data StarMatchingState = StarMatchingState
  { refStars :: [Star],
    targetStars :: [Star],
    refStarDistances :: [(Unordered Star, Double)],
    targetStarDistances :: [(Unordered Star, Double)]
  }
  deriving (Show)

data ImageInfo = ImageInfo
  { imageWidth :: Int,
    imageHeight :: Int
  }
  deriving (Show)

minPairsToBicubic :: Int
minPairsToBicubic = 40

minPairsToBiSquared :: Int
minPairsToBiSquared = 25

getTransformationType :: Int -> TransformationType
getTransformationType nrVotes
  | nrVotes >= minPairsToBicubic = TT_BICUBIC
  | nrVotes >= minPairsToBiSquared = TT_BISQUARED
  | otherwise = TT_BILINEAR

takeWhilePlusOne :: (a -> Bool) -> [a] -> [a]
takeWhilePlusOne p = go
  where
    go [] = []
    go (x : xs)
      | p x = x : go xs
      | otherwise = [x]

dropWhilePlusOne :: (a -> Bool) -> [a] -> [a]
dropWhilePlusOne p = go
  where
    go [] = []
    go (x : xs)
      | p x = go xs
      | otherwise = xs

data CoordTransState = CoordTransState
  { ctsTType :: TransformationType,
    result :: Bool,
    end :: Bool
    -- nrPairs :: Int
  }
  deriving (Show)

initialCoordTransState :: CoordTransState
initialCoordTransState = CoordTransState TT_BILINEAR False False

computeCoordinatesTransformation :: [(Position, Position)] -> Maybe BilinearParameters
computeCoordinatesTransformation vps = undefined $ go initialCoordTransState
  where
    nrExtraPairs = 4 -- corners
    go state
      | not state.end && not state.result =
          let nrPairs = case state.ctsTType of
                TT_BICUBIC -> 32 + nrExtraPairs
                TT_BISQUARED -> 18 + nrExtraPairs
                _ -> 8 + nrExtraPairs
           in undefined
      | otherwise = state

computeSigmaClippingTransformation :: [(Star, Star)] -> TransformationType -> Maybe BilinearParameters
computeSigmaClippingTransformation votingPairs ttType =
  let areCornersLocked = False
   in if areCornersLocked
        then undefined
        else computeCoordinateTransformation votingPairs ttType

computeMatchingTriangleTransformation :: Maybe BilinearParameters
computeMatchingTriangleTransformation = error "implement ComputeMatchingTriangleTransformation"

computeCoordinateTransformation :: [(Star, Star)] -> TransformationType -> Maybe BilinearParameters
computeCoordinateTransformation votingPairs ttType =
  let nrPairs = case ttType of
        TT_BICUBIC -> 32
        TT_BISQUARED -> 18
        _ -> 8
   in undefined

computeTransformation :: [(Star, Star)] -> TransformationType -> Maybe BilinearParameters
computeTransformation votingPairs ttType =
  let
   in undefined

solve :: LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
solve ref tgt =
  let m = ref <> LA.tr tgt
      (u, sigma, v) = LA.svd m
   in u <> LA.tr' v

testFoo = do
  refStars <- map RefStar . read <$> readFile "./resources/tmp/img_1_stars.txt"
  targetStars <- map TargetStar . read <$> readFile "./resources/tmp/img_2_stars.txt"
  let Just r = computeLargeTriangleTransformation refStars targetStars
  print $ length r
  let uniques = M.toList $ M.fromListWith const r
  print $ length uniques

-- let testStars = uniques
-- let (refVector, tgtVector) =
--       bimap positionListToVector positionListToVector $
--         unzip $
--           map (bimap position position) testStars
-- let r = solve refVector tgtVector
--     diff = refVector - (r <> tgtVector)
-- print diff

positionListToVector :: [Position] -> LA.Matrix Double
positionListToVector ps =
  LA.matrix (length ps) $
    concat $
      transpose $
        map (\p -> map fromIntegral [p.x, p.y]) ps
