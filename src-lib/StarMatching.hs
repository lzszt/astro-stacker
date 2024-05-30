{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module StarMatching where

import Codec.Picture qualified as P
import Data.List
import Data.Map.Strict qualified as Map
import Locating
import Types

computeStarDistances :: [Star] -> (Map.Map (Unordered Star) Double, Double)
computeStarDistances stars =
  foldl'
    ( \(distanceMap, maxDistance) (starPair, starDistance) ->
        (Map.insert starPair starDistance distanceMap, max maxDistance starDistance)
    )
    (Map.empty, 0)
    [(mkUnordered s1 s2, distance s1.starPosition s2.starPosition) | s1 <- stars, s2 <- stars]

computeMatching :: [Star] -> [Star] -> [(Star, Star)]
computeMatching stars1 stars2 =
  let sortedStars1 = sortOn (.starRadius) stars1
      sortedStars2 = sortOn (.starRadius) stars2
   in go sortedStars1 sortedStars2
  where
    go [] _ = []
    go (s1 : sts1) sts2 =
      case findClosestMatch s1 sts2 of
        Nothing -> go sts1 sts2
        Just (s2, remainingStars) -> (s1, s2) : go sts1 remainingStars

findClosestMatch :: Star -> [Star] -> Maybe (Star, [Star])
findClosestMatch Star {..} indexedStars =
  case sortOn starRelevancy indexedStars of
    [] -> Nothing
    (mostRelevantStar : remainingStars)
      | mostRelevantStar.starRadius == starRadius -> Just (mostRelevantStar, remainingStars)
      | otherwise -> Nothing
  where
    starRelevancy :: Star -> (Double, Double)
    starRelevancy s = (s.starRadius, distance s.starPosition starPosition)

test :: IO ()
test = do
  Right (P.ImageY8 lumaTiff6@P.Image {..}) <- P.readTiff "./resources/tmp/PIA17005_luma.tiff"
  let lumaTiff = P.pixelMap pixel8ToPixel16 lumaTiff6
  putStrLn $ "Width: " <> show imageWidth
  putStrLn $ "Height: " <> show imageHeight

  let stars = locateStarsDSS lumaTiff
  print $ length stars
  let (starDists, maxStarDistance) = computeStarDistances stars
  print maxStarDistance

-- print starDists

-- P.writeTiff "./resources/tmp/DSC00540_debug.tiff" $ drawWannabeStars lumaTiff wannabes
