{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Locating where

import Codec.Picture qualified as P
import Codec.Picture.Types qualified as P
import Control.Monad
import Data.Bits
import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Vector.Storable qualified as VS
import Data.Word
import ImageUtils
import System.Random
import Types

flattenImage ::
  forall a.
  (Bounded a) =>
  (Integral a) =>
  (P.Pixel a) =>
  P.Image a ->
  P.Image a
flattenImage img@P.Image {..} =
  P.pixelMap
    ( \p ->
        if p >= avgPixelValue
          then
            if p >= maxBound `div` 100 * 95
              then p
              else p - avgPixelValue
          else 0
    )
    img
  where
    pxCount = fromIntegral $ imageWidth * imageHeight
    maxVal = fromIntegral @a @Double maxBound
    avgPixelValue =
      round $
        P.pixelFold
          ( \acc _x _y p ->
              let p' = fromIntegral p
               in if p' >= maxVal
                    then acc
                    else acc + p' / pxCount
          )
          0
          img

data WannabeStar = WannabeStar
  { posX :: Int,
    posY :: Int,
    meanRadius :: Double
  }
  deriving (Show, Eq, Ord)

-- https://github.com/deepskystacker/DSS/blob/4fe3e59bf4c9d4167221e957617e43cd26da4ca4/DeepSkyStackerKernel/Stars.h#L112C54-L112C66
radiusFactor :: Double
radiusFactor = 2.35 / 1.5

withinStar :: Int -> Int -> WannabeStar -> Bool
withinStar x y WannabeStar {..} =
  distance <= meanRadius * radiusFactor
  where
    distance = sqrt $ fromIntegral $ (posX - x) ^ 2 + (posY - y) ^ 2

data DirectionState = DirectionState
  { brighterPixel :: Bool,
    mainOk :: Bool,
    maxRadius :: Int
  }
  deriving (Show)

initialDirectionState :: DirectionState
initialDirectionState =
  DirectionState
    { brighterPixel = False,
      mainOk = True,
      maxRadius = 0
    }

data PixelDirection = PixelDirection
  { intensity :: Word16,
    radius :: Int,
    nrBrighterPixels :: Int,
    pxOk :: Int,
    dirX :: Int,
    dirY :: Int
  }
  deriving (Show)

mkPixelDirection :: Int -> Int -> PixelDirection
mkPixelDirection dX dY =
  PixelDirection
    { intensity = 0,
      radius = 0,
      nrBrighterPixels = 0,
      pxOk = 2,
      dirX = dX,
      dirY = dY
    }

concentricCircles ::
  Int ->
  Word16 ->
  Word16 ->
  (DirectionState, [PixelDirection]) ->
  [PixelDirection] ->
  (DirectionState, [PixelDirection])
concentricCircles testedRadius backgroundIntensity pixelIntensity =
  go
  where
    go acc [] = acc
    go (ds@DirectionState {..}, pds) (pd : restPds)
      | brighterPixel = (ds, pd : pds <> restPds)
      | pd.pxOk > 0 =
          let (newDS, newPd) =
                if pd.intensity - backgroundIntensity < (pixelIntensity - backgroundIntensity) `shiftR` 2
                  then
                    ( ds {maxRadius = max maxRadius testedRadius},
                      pd {radius = testedRadius, pxOk = pd.pxOk - 1}
                    )
                  else
                    if fromIntegral @_ @Double pd.intensity > 1.05 * fromIntegral pixelIntensity
                      then (ds {brighterPixel = True}, pd)
                      else
                        if pd.intensity > pixelIntensity
                          then (ds, pd {nrBrighterPixels = pd.nrBrighterPixels + 1})
                          else (ds, pd)
           in go
                ( if newPd.pxOk > 0
                    then (newDS {mainOk = True}, newPd : pds)
                    else (newDS, newPd : pds)
                )
                restPds
      | otherwise = go (ds {brighterPixel = pd.nrBrighterPixels > 2 || ds.brighterPixel}, pd : pds) restPds

isWannabeStar :: P.Image Word16 -> Word16 -> Int -> Int -> Word16 -> (DirectionState, [PixelDirection])
isWannabeStar img backgroundIntensity x y pixelIntensity =
  let directions = map (uncurry mkPixelDirection) [(0, -1), (1, 0), (0, 1), (-1, 0), (1, -1), (1, 1), (-1, 1), (-1, -1)]
      testRadii = [1 .. maxStarSize]
   in foldl'
        ( \acc@(ds@DirectionState {..}, dirs) testedRadius ->
            if mainOk && not brighterPixel
              then
                let newDirs = map (\dir -> dir {intensity = pixelAtDefault 0 img (x + dir.dirX * testedRadius) (y + dir.dirY * testedRadius)}) dirs
                 in concentricCircles
                      testedRadius
                      backgroundIntensity
                      pixelIntensity
                      (ds {mainOk = False}, [])
                      newDirs
              else acc
        )
        (initialDirectionState, directions)
        testRadii

generateWannabe :: Int -> Int -> Int -> [PixelDirection] -> Maybe WannabeStar
generateWannabe x y radiusDelta pds =
  let wannabeStarOk =
        all (<= radiusDelta) $
          [abs ((pds !! k1).radius - (pds !! k2).radius) | k1 <- [0 .. 3], k2 <- [0 .. 3], k1 /= k2]
            <> [abs ((pds !! k1).radius - (pds !! k2).radius) | k1 <- [4 .. 7], k2 <- [4 .. 7], k1 /= k2]

      meanRadius1 = (/ 4) $ fromIntegral $ sum $ map (.radius) $ take 4 pds
      meanRadius2 = (* sqrt 2) . (/ 4) $ fromIntegral $ sum $ map (.radius) $ drop 4 pds
   in if wannabeStarOk
        then Just $ WannabeStar x y ((meanRadius1 + meanRadius2) / 2)
        else Nothing

type StarStructure = Map.Map Int (Set.Set WannabeStar)

isWithinStar :: Int -> Int -> StarStructure -> Bool
isWithinStar x y wss =
  let maxStarRadius = ceiling $ fromIntegral maxStarSize * radiusFactor
      nearPixel ws =
        ws.posX >= x - maxStarRadius
          && ws.posX <= x + maxStarRadius
          && ws.posY >= y - maxStarRadius
          && ws.posY <= y + maxStarRadius
   in any (\ws -> nearPixel ws && withinStar x y ws) $ fromMaybe Set.empty $ wss Map.!? (x `div` maxStarSize)

addToStarStructure :: WannabeStar -> StarStructure -> StarStructure
addToStarStructure ws =
  let minX = (ws.posX - ceiling (ws.meanRadius * radiusFactor)) `div` maxStarSize

      maxX = (ws.posX + ceiling (ws.meanRadius * radiusFactor)) `div` maxStarSize
      insertAt =
        Map.alter
          ( \case
              Nothing -> Just $ Set.singleton ws
              Just wss -> Just $ Set.insert ws wss
          )
   in if minX == maxX
        then insertAt minX
        else insertAt minX . insertAt (ws.posX `div` maxStarSize) . insertAt maxX

intensityHistogram :: P.Image Word16 -> Map.Map Word16 Word16
intensityHistogram =
  VS.foldl'
    ( \acc pixelIntensity ->
        Map.alter
          (Just . maybe 1 (+ 1))
          pixelIntensity
          acc
    )
    Map.empty
    . P.imageData

accumulateTill :: Word16 -> Map.Map Word16 Word16 -> Word16
accumulateTill cutoffSamples =
  fst
    . foldl'
      ( \(!acc@(_, valuesTillNow)) (pixelValue, pixelCount) ->
          if valuesTillNow < cutoffSamples
            then (pixelValue, valuesTillNow + pixelCount)
            else acc
      )
      (0, 0)
    . Map.toAscList

locateStarsDSS :: P.Image Word16 -> [Star]
locateStarsDSS img@P.Image {..} =
  let histogram = intensityHistogram img
      maxIntensity = fst $ Map.findMax histogram
      countHalfValues = fromIntegral $ ((imageHeight - 1) * (imageWidth - 1)) `div` 2
      background = accumulateTill countHalfValues histogram
      minLuminancy = round @Double $ 0.1 * fromIntegral (maxBound @Word16)
      intensityThreshold = minLuminancy + background
      deltaRadii = [0 .. 3]
   in if maxIntensity >= intensityThreshold
        then
          map wannabeToStar $
            Set.toList $
              Map.foldl' Set.union Set.empty $
                foldl'
                  ( \wannabes radiusDelta ->
                      P.pixelFold
                        ( \wannabeStars x y pixelIntensity ->
                            if
                              | pixelIntensity < intensityThreshold -> wannabeStars
                              | isWithinStar x y wannabeStars -> wannabeStars
                              | otherwise ->
                                  let (DirectionState {..}, pds) = isWannabeStar img background x y pixelIntensity
                                   in if not mainOk && not brighterPixel && maxRadius > 2
                                        then case generateWannabe x y radiusDelta pds of
                                          Nothing -> wannabeStars
                                          Just newStar -> addToStarStructure newStar wannabeStars
                                        else wannabeStars
                        )
                        wannabes
                        img
                  )
                  Map.empty
                  deltaRadii
        else []

wannabeToStar :: WannabeStar -> Star
wannabeToStar WannabeStar {..} = Star (Position posX posY) meanRadius

circlePixels :: Double -> Map.Map Int (Set.Set Int)
circlePixels r =
  Map.fromListWith
    Set.union
    [ (round (sin angle * r), Set.singleton $ round (cos angle * r))
      | angle <- map (\a -> a / 180 * pi) [0, 5 .. 360]
    ]

circlesPxs = map (circlePixels . fromIntegral) [2 .. maxStarSize]

translateCircle x y = Map.mapKeys (+ x) . Map.map (Set.map (+ y))

drawStars :: P.Image Word16 -> [Star] -> IO (P.Image P.PixelRGB16)
drawStars img@P.Image {..} stars = do
  let centers = sort $ map (\s -> (s.starPosition.x, s.starPosition.y)) stars
      circles = Map.unionsWith Set.union $ map (\s -> translateCircle s.starPosition.x s.starPosition.y $ circlesPxs !! round s.starRadius) stars
  targetImg <- P.newMutableImage imageWidth imageHeight
  void $
    P.pixelFoldM
      ( \(cs, crs) x y p ->
          case elemRemove (x, y) cs of
            (True, restCs) -> do
              P.writePixel targetImg x y (P.PixelRGB16 0 0 maxBound)
              pure (restCs, crs)
            (False, restCs) ->
              case mapElemRemove (x, y) crs of
                (True, restCrs) -> do
                  P.writePixel targetImg x y (P.PixelRGB16 maxBound 0 0)
                  pure (restCs, restCrs)
                (False, restCrs) -> do
                  P.writePixel targetImg x y (P.PixelRGB16 p p p)
                  pure (restCs, restCrs)
      )
      (centers, circles)
      img
  P.unsafeFreezeImage targetImg

mapElemRemove :: (Ord a) => (Ord b) => (a, b) -> Map.Map a (Set.Set b) -> (Bool, Map.Map a (Set.Set b))
mapElemRemove (x, y) m =
  case Map.splitLookup x m of
    (smaller, Nothing, bigger)
      | Map.null smaller -> (False, bigger)
      | Map.null bigger -> (False, smaller)
      | otherwise -> (False, Map.union smaller bigger)
    (xSmaller, Just ySet, xBigger)
      | Map.null xSmaller -> case Set.splitMember y ySet of
          (ySmaller, res, yBigger)
            | Set.null ySmaller,
              Set.null yBigger ->
                (res, xBigger)
            | Set.null ySmaller -> (res, Map.insert x yBigger xBigger)
            | Set.null yBigger -> (res, Map.insert x ySmaller xBigger)
            | otherwise -> (res, Map.insert x (Set.union ySmaller yBigger) xBigger)
      | Map.null xBigger -> case Set.splitMember y ySet of
          (ySmaller, res, yBigger)
            | Set.null ySmaller,
              Set.null yBigger ->
                (res, xSmaller)
            | Set.null ySmaller -> (res, Map.insert x yBigger xSmaller)
            | Set.null yBigger -> (res, Map.insert x ySmaller xSmaller)
            | otherwise -> (res, Map.insert x (Set.union ySmaller yBigger) xSmaller)
      | otherwise ->
          case Set.splitMember y ySet of
            (ySmaller, res, yBigger)
              | Set.null ySmaller,
                Set.null yBigger ->
                  (res, Map.union xSmaller xBigger)
              | Set.null ySmaller -> (res, Map.insert x yBigger $ Map.union xSmaller xBigger)
              | Set.null yBigger -> (res, Map.insert x ySmaller $ Map.union xSmaller xBigger)
              | otherwise -> (res, Map.insert x (Set.union ySmaller yBigger) $ Map.union xSmaller xBigger)

elemRemove :: (Eq a) => a -> [a] -> (Bool, [a])
elemRemove _ [] = (False, [])
elemRemove x (y : ys)
  | x == y = (True, ys)
  | otherwise = (y :) <$> elemRemove x ys

maxStarSize :: Int
maxStarSize = 50

pixel8ToPixel16 :: Word8 -> Word16
pixel8ToPixel16 =
  round @Double
    . (fromIntegral @Word16 @Double maxBound *)
    . (/ fromIntegral @Word8 @Double maxBound)
    . fromIntegral

generateTransform :: IO Alignment
generateTransform = do
  offX <- randomRIO (-10, 10)
  offY <- randomRIO (-10, 10)
  rot <- randomRIO (-0.1, 0.1)
  pure $ Alignment offX offY rot

generateTransforms :: [t] -> IO [(t, Alignment)]
generateTransforms = mapM (\img -> (img,) <$> generateTransform)

applyTransform :: Alignment -> Tiff -> Tiff
applyTransform alg img =
  P.generateImage generatePixel img.imageWidth img.imageHeight
  where
    generatePixel x' y' =
      let Position {..} = applyAlignment alg $ Position x' y'
       in if 0 <= x
            && x < img.imageWidth
            && 0 <= y
            && y < img.imageHeight
            then P.pixelAt img x y
            else P.PixelRGB16 0 0 0

generateTestTiffs :: IO ()
generateTestTiffs = do
  let n = 100
  Right (P.ImageRGB8 original) <- P.readTiff "./PIA17005.tiff"

  withTransforms <- generateTransforms [1 .. n]
  mapM_ (print . snd) withTransforms

  mapM_
    ( \(i, transform) ->
        P.writeTiff ("./resources/test_lights/img_" <> show i <> ".tiff") $
          applyTransform transform $
            P.pixelMap
              ( \(P.PixelRGB8 r g b) ->
                  P.PixelRGB16
                    (pixel8ToPixel16 r)
                    (pixel8ToPixel16 g)
                    (pixel8ToPixel16 b)
              )
              original
    )
    withTransforms

test :: IO ()
test = do
  Right (P.ImageY8 lumaTiff8@P.Image {..}) <- P.readTiff "./resources/tmp/PIA17005_luma.tiff"
  let lumaTiff = P.pixelMap pixel8ToPixel16 lumaTiff8
  putStrLn $ "Width: " <> show imageWidth
  putStrLn $ "Height: " <> show imageHeight

  let alls = locateStarsDSS lumaTiff
  print $ length alls

  let wannabes = sort alls
  -- mapM_ print wannabes

  P.writeTiff "./resources/tmp/PIA17005_luma_debug.tiff" =<< drawStars lumaTiff wannabes

---------------------------------------------------------------
-- Workflow locating stars (DSS)
---------------------------------------------------------------

-- calculate maximum intensity of all pixels
-- calculate the fiftyth percentile of all intensities
-- background is ratio of fifty percentile intensity to maxBound
-- intensity threshold is minLuninancy + background where minLuminancy is a config parameter typically set to 10?
-- for deltaRadius <- [0..4]
-- iterate over image
--    get pixel intensity
--    if intensity is >= intensityThreshold

---------------------------------------------------------------
-- Benchmarking
---------------------------------------------------------------

---------------------------------------------------------------
-- GHC 9.6.4
---------------------------------------------------------------

-- Width: 4095
-- Height: 2842
-- 1449

-- ________________________________________________________
-- Executed in  103.53 secs    fish           external
--    usr time  106.75 secs    1.16 millis  106.75 secs
--    sys time   23.52 secs    0.10 millis   23.52 secs

---------------------------------------------------------------
-- GHC 9.6.4 + -fspecialise-aggressively
---------------------------------------------------------------

-- Width: 4095
-- Height: 2842
-- 1449

-- ________________________________________________________
-- Executed in  102.54 secs    fish           external
--    usr time  105.87 secs    1.27 millis  105.87 secs
--    sys time   22.77 secs    0.15 millis   22.77 secs

---------------------------------------------------------------
-- GHC 9.8.2 + -fspecialise-aggressively
---------------------------------------------------------------

-- Width: 4095
-- Height: 2842
-- 1449

-- ________________________________________________________
-- Executed in  117.51 secs    fish           external
--    usr time  115.17 secs  496.00 micros  115.17 secs
--    sys time   25.45 secs  738.00 micros   25.45 secs