{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module StarMatching where

import Codec.Picture qualified as P
import Data.List
import Data.Map.Strict qualified as Map
import Data.Ord
import Locating
import Types

newtype Unordered a = Unordered {getUnordered :: (a, a)}
  deriving (Show)

mkUnordered :: Ord a => a -> a -> Unordered a
mkUnordered x y
  | x <= y = Unordered (x, y)
  | otherwise = Unordered (y, x)

instance Eq a => Eq (Unordered a) where
  Unordered p1 == Unordered p2 = p1 == p2

instance Ord a => Ord (Unordered a) where
  compare (Unordered p1) (Unordered p2) = compare p1 p2

computeStarDistances :: [Star] -> (Map.Map (Unordered Star) Double, Double)
computeStarDistances stars =
  foldl'
    ( \(distanceMap, maxDistance) (starPair, starDistance) ->
        (Map.insert starPair starDistance distanceMap, max maxDistance starDistance)
    )
    (Map.empty, 0)
    [(mkUnordered s1 s2, distance s1.starPosition s2.starPosition) | s1 <- stars, s2 <- stars]

computeMatching :: [Star] -> [Star] -> [(Int, Int)]
computeMatching stars1 stars2 =
  let indexedStars1 = sortOn ((.starRadius) . snd) $ zip [0 ..] stars1
      indexedStars2 = sortOn ((.starRadius) . snd) $ zip [0 ..] stars2
   in go indexedStars1 indexedStars2
  where
    go [] _ = []
    go ((i, s1) : sts1) sts2 =
      case findClosestMatch s1 sts2 of
        Nothing -> go sts1 sts2
        Just (j, _, remainingStars) -> (i, j) : go sts1 remainingStars

findClosestMatch :: Star -> [(Int, Star)] -> Maybe (Int, Star, [(Int, Star)])
findClosestMatch Star {..} indexedStars =
  case sortOn (starRelevancy . snd) indexedStars of
    [] -> Nothing
    ((j, mostRelevantStar) : remainingStars)
      | mostRelevantStar.starRadius == starRadius -> Just (j, mostRelevantStar, remainingStars)
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
