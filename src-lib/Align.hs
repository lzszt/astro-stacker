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
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ord
import Debug.Trace
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

initialStarMatchingState :: [Star] -> [Star] -> StarMatchingState
initialStarMatchingState ref target =
  StarMatchingState ref target (computeStarDistances ref) (computeStarDistances target)

initVotingMap :: (Ord a, Ord b) => [a] -> [b] -> M.Map (a, b) Int
initVotingMap refStars tgtStars = M.fromList [((r, t), 0) | r <- refStars, t <- tgtStars]

vpFlagActive :: Int
vpFlagActive = 0x00000001

maxStarDistanceDelta :: Double
maxStarDistanceDelta = 4.0

minPairsToBicubic :: Int
minPairsToBicubic = 40

minPairsToBiSquared :: Int
minPairsToBiSquared = 25

getTransformationType :: Int -> TransformationType
getTransformationType nrVotes
  | nrVotes >= minPairsToBicubic = TT_BICUBIC
  | nrVotes >= minPairsToBiSquared = TT_BISQUARED
  | otherwise = TT_BILINEAR

computeStarDistances :: (Located a, Ord a) => [a] -> [(Unordered a, Double)]
computeStarDistances = sortOn (Down . snd) . go
  where
    go [] = []
    go (s : stars) = map (\st -> (mkUnordered s st, calcStarDistance s st)) stars <> go stars

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

calcStarDistance :: (Located a1, Located a2) => a1 -> a2 -> Double
calcStarDistance s1 s2 = distanceSqr (position s1) (position s2)

maxRadiusDelta = 2

-- computeLargeTriangleTransformation :: [Star] -> [Star] -> Maybe BilinearParameters
-- computeLargeTriangleTransformation ::
--   (Located ref, Located target, Ord ref, Ord target) =>
--   [ref] ->
--   [target] ->
--   Maybe [(ref, target)]
computeLargeTriangleTransformation :: (IsStar a, IsStar b, Ord a, Ord b, Show a, Show b) => [a] -> [b] -> Maybe [(a, b)]
computeLargeTriangleTransformation refStars tgtStars =
  let votes = goOverStarDistances (initVotingMap refStars tgtStars) refStarDistances tgtStarDistances
   in if length votes >= length tgtStars
        then
          let remainingVotes = resolveVotes (length tgtStars) votes
              ttType = getTransformationType (length remainingVotes)
           in Just remainingVotes -- computeSigmaClippingTransformation remainingVotes ttType
        else Nothing
  where
    refStarDistances = computeStarDistances refStars
    tgtStarDistances = computeStarDistances tgtStars

    -- goOverStarDistances :: VotingMap -> [(Unordered Star, Double)] -> [(Unordered Star, Double)] -> [((Star, Star), Int)]
    goOverStarDistances vm [] _ = sortOn (Down . snd) $ M.toList vm
    goOverStarDistances vm _ [] = sortOn (Down . snd) $ M.toList vm
    goOverStarDistances
      vm
      rDists@((Unordered (refStar1, refStar2), refDistanceSqr12) : refDists)
      tDists@((Unordered (tgtStar1, tgtStar2), tgtDistanceSqr12) : tgtDists) =
        let newVM =
              if abs (tgtDistanceSqr12 - refDistanceSqr12) <= maxStarDistanceDelta
                -- && ( ( abs ((starRadius $ toStar refStar1) - (starRadius $ toStar tgtStar1)) <= maxStarDistanceDelta
                --          && abs ((starRadius $ toStar refStar2) - (starRadius $ toStar tgtStar2)) <= maxStarDistanceDelta
                --      )
                --        || ( abs ((starRadius $ toStar refStar2) - (starRadius $ toStar tgtStar1)) <= maxStarDistanceDelta
                --               && abs ((starRadius $ toStar refStar1) - (starRadius $ toStar tgtStar2)) <= maxStarDistanceDelta
                --           )
                --    )
                then
                  foldl'
                    ( \acc tgtStar3 ->
                        if tgtStar3 /= tgtStar1 && tgtStar3 /= tgtStar2
                          then
                            let tgtDistanceSqr13 = calcStarDistance tgtStar1 tgtStar3
                                tgtDistanceSqr23 = calcStarDistance tgtStar2 tgtStar3
                                fRatio = max tgtDistanceSqr13 tgtDistanceSqr23 / tgtDistanceSqr12
                             in -- Filter triangle because :
                                -- Larger triangle are already used
                                -- 0.9^2 avoids many useless triangles with two big sides and one small side
                                if fRatio < 0.81
                                  then
                                    -- FIXME (felix): this should be doable in parallel and the votes merged
                                    foldl'
                                      ( \acc refStar3 ->
                                          if refStar3 /= refStar1
                                            && refStar3 /= refStar2
                                            -- && abs ((starRadius $ toStar refStar3) - (starRadius $ toStar tgtStar3)) <= maxStarDistanceDelta
                                            then
                                              let refDistanceSqr13 = calcStarDistance refStar1 refStar3
                                                  refDistanceSqr23 = calcStarDistance refStar2 refStar3
                                               in if abs (refDistanceSqr13 - tgtDistanceSqr13) < maxStarDistanceDelta
                                                    && abs (refDistanceSqr23 - tgtDistanceSqr23) < maxStarDistanceDelta
                                                    then
                                                      addVote refStar1 tgtStar1 $
                                                        addVote refStar2 tgtStar2 $
                                                          addVote refStar3 tgtStar3 acc
                                                    else
                                                      if abs (refDistanceSqr23 - tgtDistanceSqr13) < maxStarDistanceDelta
                                                        && abs (refDistanceSqr13 - tgtDistanceSqr23) < maxStarDistanceDelta
                                                        then
                                                          addVote refStar1 tgtStar2 $
                                                            addVote refStar2 tgtStar1 $
                                                              addVote refStar3 tgtStar3 acc
                                                        else acc
                                            else acc
                                      )
                                      acc
                                      refStars
                                  else acc
                          else acc
                    )
                    vm
                    tgtStars
                else vm
         in if tgtDistanceSqr12 < refDistanceSqr12
              then goOverStarDistances newVM refDists tDists
              else goOverStarDistances newVM rDists tgtDists

resolveVotes :: Int -> [((a, b), Int)] -> [(a, b)]
resolveVotes nTgtStars votes =
  let minNrVotes = max 1 $ snd $ votes !! (nTgtStars * 2 - 1)
   in map fst $ takeWhile ((>= minNrVotes) . snd) votes

getStarDist :: (Ord p) => p -> p -> M.Map (Unordered p) b -> Maybe b
getStarDist s1 s2 dists =
  let key = mkUnordered s1 s2
   in M.lookup key dists

addVote :: (Ord a, Ord b) => a -> b -> M.Map (a, b) Int -> M.Map (a, b) Int
addVote s1 s2 =
  -- trace ("vote: " <> show s1 <> "," <> show s2) $
  Map.alter
    ( \case
        Nothing -> Just 1
        Just n -> Just $ n + 1
    )
    (s1, s2)

computeMatchingTriangleTransformation :: Maybe BilinearParameters
computeMatchingTriangleTransformation = error "implement ComputeMatchingTriangleTransformation"

type VotingMap = Map.Map (Star, Star) Int

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

newtype RefStar = RefStar {unRef :: Star}
  deriving (Eq, Ord, Show, Located, IsStar)

newtype TargetStar = TargetStar {unTarget :: Star}
  deriving (Show, Located, Eq, Ord, IsStar)

locatedDistance :: (Located a, Located b) => a -> b -> Double
locatedDistance x y = distance (position x) (position y)

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
