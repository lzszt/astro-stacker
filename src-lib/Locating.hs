{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Locating where

import Codec.Picture qualified as P
import Codec.Picture.Types qualified as P
import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
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
  deriving (Show)

withinStar :: Int -> Int -> WannabeStar -> Bool
withinStar x y ws@WannabeStar {..} =
  let -- https://github.com/deepskystacker/DSS/blob/4fe3e59bf4c9d4167221e957617e43cd26da4ca4/DeepSkyStackerKernel/Stars.h#L112C54-L112C66
      radiusFactor = 2.35 / 1.5
   in distance <= meanRadius * radiusFactor
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

isWannabeStar :: P.Image Word16 -> Word16 -> Int -> Int -> Word16 -> (DirectionState, [PixelDirection])
isWannabeStar img backgroundIntensity x y pixelIntensity =
  let directions = map (uncurry mkPixelDirection) [(0, -1), (1, 0), (0, 1), (-1, 0), (1, -1), (1, 1), (-1, 1), (-1, -1)]
      testRadii = [1 .. maxStarSize]
   in foldl'
        ( \acc@(ds@DirectionState {..}, dirs) testedRadius ->
            if mainOk && not brighterPixel
              then
                let newDirs = map (\dir -> dir {intensity = pixelAtDefault 0 img (x + dir.dirX * testedRadius) (y + dir.dirY * testedRadius)}) dirs
                 in foldl'
                      ( \(ds, pds) pd ->
                          if brighterPixel
                            then (ds, pd : pds)
                            else
                              if pd.pxOk > 0
                                then
                                  let (newDS, newPd) =
                                        if fromIntegral @_ @Double (pd.intensity - backgroundIntensity) < fromIntegral (pixelIntensity - backgroundIntensity) / 4
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
                                   in if newPd.pxOk > 0
                                        then (newDS {mainOk = True}, newPd : pds)
                                        else (newDS, newPd : pds)
                                else
                                  if pd.nrBrighterPixels > 2
                                    then (ds {brighterPixel = True}, pd : pds)
                                    else (ds, pd : pds)
                      )
                      (ds {mainOk = False}, [])
                      newDirs
              else acc
        )
        (initialDirectionState, directions)
        testRadii

generateWannabe :: Int -> Int -> Int -> (DirectionState, [PixelDirection]) -> Maybe WannabeStar
generateWannabe x y radiusDelta (_, pds) =
  let wannabeStarOk =
        all (<= radiusDelta) $
          [abs ((pds !! k1).radius - (pds !! k2).radius) | k1 <- [0 .. 3], k2 <- [0 .. 3], k1 /= k2]
            <> [abs ((pds !! k1).radius - (pds !! k2).radius) | k1 <- [4 .. 7], k2 <- [4 .. 7], k1 /= k2]

      meanRadius1 = (/ 4) $ fromIntegral $ sum $ map (.radius) $ take 4 pds
      meanRadius2 = (* sqrt 2) . (/ 4) $ fromIntegral $ sum $ map (.radius) $ drop 4 pds
   in if wannabeStarOk
        then Just $ WannabeStar x y ((meanRadius1 + meanRadius2) / 2)
        else Nothing

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
            foldl'
              ( \wannabes radiusDelta ->
                  P.pixelFold
                    ( \wannabeStars x y pixelIntensity ->
                        let withinKnownStar = any (withinStar x y) wannabeStars
                         in if (pixelIntensity >= intensityThreshold) && not withinKnownStar
                              then
                                let res@(DirectionState {..}, _) = isWannabeStar img background x y pixelIntensity
                                 in if not mainOk && not brighterPixel && maxRadius > 2
                                      then case generateWannabe x y radiusDelta res of
                                        Nothing -> wannabeStars
                                        Just newStar -> newStar : wannabeStars
                                      else wannabeStars
                              else wannabeStars
                    )
                    wannabes
                    img
              )
              []
              deltaRadii
        else []

wannabeToStar :: WannabeStar -> Star
wannabeToStar WannabeStar {..} = Star (Position posX posY) meanRadius

circlePixels :: Int -> Int -> Double -> Map.Map Int (Set.Set Int)
circlePixels x y r =
  Map.fromListWith
    Set.union
    [ (x + round (sin angle * r), Set.singleton $ y + round (cos angle * r))
      | angle <- map (\a -> a / 180 * pi) [0, 5 .. 360]
    ]

drawStars :: P.Image Word16 -> [Star] -> P.Image P.PixelRGB16
drawStars img stars =
  let centers = map (\s -> (s.starPosition.x, s.starPosition.y)) stars
      circles = Map.unionsWith Set.union $ map (\s -> circlePixels s.starPosition.x s.starPosition.y s.starRadius) stars
   in P.pixelMapXY
        ( \pX pY p ->
            if (pX, pY) `elem` centers
              then P.PixelRGB16 0 0 maxBound
              else
                if pY `elem` fromMaybe Set.empty (circles Map.!? pX)
                  then P.PixelRGB16 maxBound 0 0
                  else P.PixelRGB16 p p p
        )
        img

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
  Right (P.ImageY8 lumaTiff6@P.Image {..}) <- P.readTiff "./resources/tmp/PIA17005_luma.tiff"
  let lumaTiff = P.pixelMap pixel8ToPixel16 lumaTiff6
  putStrLn $ "Width: " <> show imageWidth
  putStrLn $ "Height: " <> show imageHeight

  let alls = locateStarsDSS lumaTiff
  print $ length alls
  let wannabes = alls
  print wannabes
  P.writeTiff "./resources/tmp/DSC00540_debug.tiff" $ drawStars lumaTiff wannabes

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
