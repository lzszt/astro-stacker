{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Locating where

import Codec.Picture qualified as P
import Codec.Picture.Types qualified as P
import Control.Monad
import Data.Bits
import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Time
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word
import Debug.Trace
import Types

flattenImage ::
  forall a.
  Bounded a =>
  Integral a =>
  P.Pixel a =>
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

safePixelAt :: P.Pixel a => P.Image a -> Int -> Int -> Maybe a
safePixelAt img@P.Image {..} x y
  | x >= 0,
    x < imageWidth,
    y >= 0,
    y < imageHeight =
      Just $ P.pixelAt img x y
  | otherwise = Nothing

pixelAtDefault :: P.Pixel a => a -> P.Image a -> Int -> Int -> a
pixelAtDefault def img x y = fromMaybe def $ safePixelAt img x y

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
   in trace "found star" $
        if minX == maxX
          then insertAt minX
          else insertAt minX . insertAt (ws.posX `div` maxStarSize) . insertAt maxX

locateStarsDSS :: P.Image Word16 -> [Star]
locateStarsDSS img@P.Image {..} =
  let maxIntensity = VS.foldl1' max imageData
      histogram = Map.toAscList $ VS.foldl' (\acc pixelIntensity -> Map.insertWith (+) pixelIntensity (1 :: Word16) acc) Map.empty imageData
      countHalfValues = fromIntegral $ ((imageHeight - 1) * (imageWidth - 1)) `div` 2
      background =
        fst $
          foldl'
            ( \acc@(_, valuesTillNow) (pixelValue, pixelCount) ->
                if valuesTillNow < countHalfValues
                  then (pixelValue, valuesTillNow + pixelCount)
                  else acc
            )
            (0, 0)
            histogram
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
                            let withinKnownStar = isWithinStar x y wannabeStars
                             in if (pixelIntensity >= intensityThreshold) && not withinKnownStar
                                  then
                                    let (DirectionState {..}, pds) = isWannabeStar img background x y pixelIntensity
                                     in if not mainOk && not brighterPixel && maxRadius > 2
                                          then case generateWannabe x y radiusDelta pds of
                                            Nothing -> wannabeStars
                                            Just newStar -> addToStarStructure newStar wannabeStars
                                          else wannabeStars
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

mapElemRemove :: Ord a => Ord b => (a, b) -> Map.Map a (Set.Set b) -> (Bool, Map.Map a (Set.Set b))
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

elemRemove :: Eq a => a -> [a] -> (Bool, [a])
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

test :: IO ()
test = do
  Right (P.ImageRGB16 tiff@P.Image {..}) <- P.readTiff "./resources/lights/DSC00556.tiff"
  P.writeTiff "./resources/tmp/DSC00556_luma.tiff" $ P.extractLumaPlane tiff
  -- Right (P.ImageY8 lumaTiff6@P.Image {..}) <- P.readTiff "./resources/tmp/PIA17005_luma.tiff"
  let lumaTiff = P.extractLumaPlane tiff
  putStrLn $ "Width: " <> show imageWidth
  putStrLn $ "Height: " <> show imageHeight

  let alls = locateStarsDSS lumaTiff
  print $ length alls

  let wannabes = sort alls
  -- mapM_ print wannabes

  P.writeTiff "./resources/tmp/DSC00556_debug.tiff" =<< drawStars lumaTiff wannabes

splitIntoQuarters :: P.Image a -> (P.Image a, P.Image a, P.Image a, P.Image a)
splitIntoQuarters P.Image {..} = undefined
  where
    targetWidth = imageWidth `div` 2
    targetHeight = imageHeight `div` 2

---------------------------------------------------------------
-- Workflow locating stars (DSS)
---------------------------------------------------------------

-- calculate maximum intensity of all pixels
-- calculate the fiftyth percentile of all intensities
-- background is ratio of fifty percentile intensity to maxBound
-- intensity threshold is minLuninancy + background where minLuminancy is a config parameter tipically set to 10?
-- for deltaRadius <- [0..4]
-- iterate over image
--    get pixel intensity
--    if intensity is >= intensityThreshold

---------------------------------------------------------------
-- Benchmarking
---------------------------------------------------------------

---------------------------------------------------------------
-- Start
---------------------------------------------------------------

-- Width: 4095
-- Height: 2842
-- 1449

-- ________________________________________________________
-- Executed in   29.21 secs    fish           external
--    usr time   29.08 secs    1.32 millis   29.08 secs
--    sys time    0.10 secs    0.24 millis    0.10 secs

---------------------------------------------------------------
-- filter stars around current position
---------------------------------------------------------------

-- Width: 4095
-- Height: 2842
-- 1449

-- ________________________________________________________
-- Executed in   15.64 secs    fish           external
--    usr time   15.58 secs  776.00 micros   15.57 secs
--    sys time    0.07 secs    0.00 micros    0.07 secs

---------------------------------------------------------------
-- split wannabe stars into bands
---------------------------------------------------------------

-- Width: 4095
-- Height: 2842
-- 1449

-- ________________________________________________________
-- Executed in    6.46 secs    fish           external
--    usr time    6.40 secs    0.07 millis    6.40 secs
--    sys time    0.08 secs    1.03 millis    0.08 secs