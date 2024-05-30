{-# LANGUAGE LambdaCase #-}

module Matching where

import Data.List
import Data.Map.Strict qualified as M
import Data.Ord
import Types

matchStars :: [RefStar] -> [TargetStar] -> [(RefStar, TargetStar)]
matchStars = undefined

computeStarDistances :: (Located a, Ord a) => [a] -> [(Unordered a, Double)]
computeStarDistances = sortOn (Down . snd) . go
  where
    go [] = []
    go (s : stars) = map (\st -> (mkUnordered s st, calcStarDistance s st)) stars <> go stars

calcStarDistance :: (Located a1, Located a2) => a1 -> a2 -> Double
calcStarDistance s1 s2 = distanceSqr (position s1) (position s2)

maxRadiusDelta = 2

getStarDist :: (Ord p) => p -> p -> M.Map (Unordered p) b -> Maybe b
getStarDist s1 s2 dists =
  let key = mkUnordered s1 s2
   in M.lookup key dists

addVote :: (Ord a, Ord b) => a -> b -> M.Map (a, b) Int -> M.Map (a, b) Int
addVote =
  curry
    ( M.alter
        ( \case
            Nothing -> Just 1
            Just n -> Just $ n + 1
        )
    )

addVotes :: (Foldable t, Ord a, Ord b) => t (a, b) -> M.Map (a, b) Int -> M.Map (a, b) Int
addVotes votingPairs vm =
  foldl' (\acc (a, b) -> addVote a b acc) vm votingPairs

maxStarDistanceDelta :: Double
maxStarDistanceDelta = 4.0

{-# SPECIALIZE computeLargeTriangleTransformation :: [RefStar] -> [TargetStar] -> [(RefStar, TargetStar)] #-}
{-# SPECIALIZE computeLargeTriangleTransformation :: [Star] -> [Star] -> [(Star, Star)] #-}
computeLargeTriangleTransformation :: (IsStar a, IsStar b, Ord a, Ord b, Show a, Show b) => [a] -> [b] -> [(a, b)]
computeLargeTriangleTransformation refStars tgtStars =
  let votes = goOverStarDistances M.empty refStarDistances tgtStarDistances
   in resolveVotes (length tgtStars) votes
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
                                                  votes =
                                                    if abs (refDistanceSqr13 - tgtDistanceSqr13) < maxStarDistanceDelta
                                                      && abs (refDistanceSqr23 - tgtDistanceSqr23) < maxStarDistanceDelta
                                                      then
                                                        [ (refStar1, tgtStar1),
                                                          (refStar2, tgtStar2),
                                                          (refStar3, tgtStar3)
                                                        ]
                                                      else
                                                        if abs (refDistanceSqr23 - tgtDistanceSqr13) < maxStarDistanceDelta
                                                          && abs (refDistanceSqr13 - tgtDistanceSqr23) < maxStarDistanceDelta
                                                          then
                                                            [ (refStar1, tgtStar2),
                                                              (refStar2, tgtStar1),
                                                              (refStar3, tgtStar3)
                                                            ]
                                                          else []
                                               in addVotes votes acc
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
  let minNrVotes = maybe 1 (max 1 . snd) $ votes !? (nTgtStars * 2 - 1)
   in map fst $ takeWhile ((>= minNrVotes) . snd) votes