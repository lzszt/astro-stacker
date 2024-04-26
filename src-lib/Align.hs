{-# LANGUAGE OverloadedRecordDot #-}

module Align where

import Control.Applicative
import Control.Arrow
import Data.Foldable
import Data.Function
import Data.List
import Data.Map.Strict qualified as Map
import Debug.Trace
import Types

computeAlignment :: AlignmentMode -> [(Position, Position)] -> Alignment
computeAlignment alignmentMode starPairs =
  case alignmentMode of
    NoAlignment -> Alignment 0 0 0
    TranslationOnly -> computeTranslation starPairs
    TranslateAndRotate -> error "implement TranslateAndRotate"

computeTranslation :: [(Position, Position)] -> Alignment
computeTranslation [] = error "Cannot compute translation without star pairs"
computeTranslation stars =
  let (xMinRef, xMaxRef) = minMax $ map (x . fst) stars
      (yMinRef, yMaxRef) = minMax $ map (y . fst) stars
      (xMinTar, xMaxTar) = minMax $ map (x . snd) stars
      (yMinTar, yMaxTar) = minMax $ map (y . snd) stars
      xMinDiff = traceShowId $ xMinRef - xMaxTar
      xMaxDiff = traceShowId $ xMaxRef - xMinTar
      yMinDiff = traceShowId $ yMinRef - yMaxTar
      yMaxDiff = traceShowId $ yMaxRef - yMinTar
      potentialAlignments = [Alignment dX dY 0 | dX <- [xMinDiff .. xMaxDiff], dY <- [yMinDiff .. yMaxDiff]]

      alignmentScore alg = sum $ map (uncurry distanceSqr . second (applyAlignment alg)) stars
   in minimumBy (compare `on` alignmentScore) potentialAlignments

minMax :: (Ord a) => [a] -> (a, a)
minMax [] = error "Cannot get min/max from empty list"
minMax (x : xs) =
  go (x, x) xs
  where
    go acc [] = acc
    go acc@(cMin, cMax) (y : ys)
      | y < cMin = go (y, cMax) ys
      | y <= cMax = go acc ys
      | otherwise = go (cMin, y) ys

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

data StarMatchingState = StarMatchingState
  { refStars :: [Star],
    targetStars :: [Star],
    refStarDistances :: [StarDist],
    targetStarDistances :: [StarDist]
  }
  deriving (Show)

initialStarMatchingState :: [Star] -> [Star] -> StarMatchingState
initialStarMatchingState ref target =
  StarMatchingState ref target (computeStarDistances ref) (computeStarDistances target)

data StarDist = StarDist
  { star1 :: Star,
    star2 :: Star,
    interStarDistance :: Double
  }
  deriving (Show)

vpFlagActive :: Int
vpFlagActive = 0x00000001

maxStarDistanceDelta :: Double
maxStarDistanceDelta = 2.0

getTransformationType :: TransformationType
getTransformationType = TT_LINEAR

computeStarDistances :: [Star] -> [StarDist]
computeStarDistances = sortBy (flip compare `on` (.interStarDistance)) . go
  where
    go [] = []
    go (s : stars) = map (\st -> StarDist s st (starDistance s st)) stars <> go stars

takeWhilePlusOne :: (a -> Bool) -> [a] -> [a]
takeWhilePlusOne p = go
  where
    go [] = []
    go (x : xs)
      | p x = x : go xs
      | otherwise = [x]

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

computeSigmaClippingTransformation :: [(Unordered Star, Int)] -> Maybe BilinearParameters
computeSigmaClippingTransformation xs =
  traceShow xs $
    if length (show xs) >= 1000
      then Nothing
      else Just undefined

computeLargeTriangleTransformation :: StarMatchingState -> Maybe BilinearParameters
computeLargeTriangleTransformation sms =
  let votingPairs =
        sortBy (flip compare `on` snd) $
          Map.toList $
            goOverStarDistances
              (initVotingMap sms)
              sms.refStarDistances
              sms.targetStarDistances
   in if length votingPairs >= length sms.targetStars
        then
          let minNumberOfVotes = max 1 $ snd (votingPairs !! (2 * length sms.targetStars - 1))
              relevantVotingPairs = takeWhilePlusOne ((>= minNumberOfVotes) . snd) votingPairs
           in case computeSigmaClippingTransformation relevantVotingPairs of
                Nothing -> Nothing
                Just bp
                  | bp.tType == TT_LINEAR ->
                      Just bp {a3 = 0, b3 = 0}
                  | otherwise -> Just bp
        else Nothing
  where
    goOverStarDistances :: VotingMap -> [StarDist] -> [StarDist] -> VotingMap
    goOverStarDistances acc [] _ = acc
    goOverStarDistances acc _ [] = acc
    goOverStarDistances acc refs@(refDist : refDists) tars@(tarDist : tarDists)
      | abs (refDist.interStarDistance - tarDist.interStarDistance) <= maxStarDistanceDelta =
          let refStar1 = refDist.star1
              refStar2 = refDist.star2

              tarStar1 = tarDist.star1
              tarStar2 = tarDist.star2
              tarStars3 =
                [ tarStar3
                  | tarStar3 <- sms.targetStars,
                    tarStar3 /= tarStar1,
                    tarStar3 /= tarStar2
                ]
              refStars3 =
                [ refStar3
                  | refStar3 <- sms.refStars,
                    refStar3 /= refStar1,
                    refStar3 /= refStar2
                ]
              newAcc =
                foldl
                  ( \vm tarStar3 ->
                      let tarDist13 = starDistance tarStar1 tarStar3
                          tarDist23 = starDistance tarStar2 tarStar3
                          ratio = max tarDist13 tarDist23 / tarDist.interStarDistance
                       in if ratio >= 0.9
                            then vm
                            else
                              foldl
                                ( \vm' refStar3 ->
                                    let refDist13 = starDistance refStar1 refStar3
                                        refDist23 = starDistance refStar2 refStar3
                                     in ( if abs (refDist13 - tarDist13) < maxStarDistanceDelta
                                            && abs (refDist23 - tarDist23) < maxStarDistanceDelta
                                            then
                                              addVote refStar1 tarStar1
                                                . addVote refStar2 tarStar2
                                                . addVote refStar3 tarStar3
                                            else
                                              if abs (refDist23 - tarDist13) < maxStarDistanceDelta
                                                && abs (refDist13 - tarDist23) < maxStarDistanceDelta
                                                then
                                                  addVote refStar1 tarStar2
                                                    . addVote refStar2 tarStar1
                                                    . addVote refStar3 tarStar3
                                                else id
                                        )
                                          vm'
                                )
                                vm
                                refStars3
                  )
                  acc
                  tarStars3
           in goOverStarDistances newAcc refDists tarDists
      | refDist.interStarDistance < tarDist.interStarDistance =
          goOverStarDistances acc refs tarDists
      | otherwise = goOverStarDistances acc refDists tars

addVote :: Star -> Star -> VotingMap -> VotingMap
addVote s1 s2 =
  -- trace ("vote: " <> show s1 <> "," <> show s2) $
  Map.update (Just . (+ 1)) (mkUnordered s1 s2)

computeMatchingTriangleTransformation :: Maybe BilinearParameters
computeMatchingTriangleTransformation = error "implement ComputeMatchingTriangleTransformation"

type VotingMap = Map.Map (Unordered Star) Int

initVotingMap :: StarMatchingState -> VotingMap
initVotingMap sms = Map.fromList [(mkUnordered ref tar, 0) | ref <- sms.refStars, tar <- sms.targetStars]

computeCoordinateTransformation :: StarMatchingState -> Maybe BilinearParameters
computeCoordinateTransformation sms =
  case getTransformationType of
    TT_NONE -> Nothing
    _
      | length sms.refStars >= 8,
        length sms.targetStars >= 8 ->
          computeLargeTriangleTransformation sms <|> computeMatchingTriangleTransformation
      | otherwise -> Nothing

testRefStars :: [Star]
testRefStars = take 15 stars

stars :: [Star]
stars =
  [ Star (Position 251 34) 10,
    Star (Position 462 25) 4,
    Star (Position 894 446) 5,
    Star (Position 810 659) 30,
    Star (Position 716 918) 20,
    Star (Position 2 253) 44,
    Star (Position 459 541) 23,
    Star (Position 72 145) 42,
    Star (Position 351 700) 26,
    Star (Position 941 827) 48,
    Star (Position 137 885) 18,
    Star (Position 110 710) 39,
    Star (Position 889 844) 47,
    Star (Position 245 528) 43,
    Star (Position 518 603) 17,
    Star (Position 95 810) 17,
    Star (Position 761 193) 39,
    Star (Position 872 143) 27,
    Star (Position 270 567) 22,
    Star (Position 96 582) 28
  ]

testTargetStars :: [Star]
testTargetStars = drop 5 $ map (\s -> s {starPosition = Position (s.starPosition.x + 13) (s.starPosition.y + 13)}) testRefStars

test :: Maybe BilinearParameters
test =
  let sms = initialStarMatchingState testRefStars testTargetStars
   in computeCoordinateTransformation sms
