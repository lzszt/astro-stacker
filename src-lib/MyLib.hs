{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyLib where

import Codec.Picture qualified as P
import Codec.Picture.Types qualified as P
import Control.Concurrent.Async qualified as Async
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.Time (diffUTCTime)
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable qualified as Vector
import Debug.Trace
import Effects.Cache qualified as Cache
import Effects.Logging qualified as Log
import Effects.Time qualified as Time
import Polysemy
import Polysemy.Reader qualified as Reader
import System.Directory
import System.FilePath
import System.IO (readFile')

data StarLocation = StarLocation
  { starLocationX :: Int,
    starLocationY :: Int
  }
  deriving (Show, Eq, Ord)

data Alignment = Alignment
  { offsetX :: Int,
    offsetY :: Int,
    rotation :: Double
  }
  deriving (Show)

data Raw

data Master

data Clean

data StarsLocated

data Aligned

type Tiff = P.Image P.PixelRGB16

data Image a where
  Raw :: String -> Tiff -> Image Raw
  Master :: String -> Tiff -> Image Master
  Clean :: String -> Tiff -> Image Clean
  StarsLocated :: String -> [StarLocation] -> Tiff -> Image StarsLocated
  Aligned :: String -> Tiff -> Image Aligned

getTiff :: Image a -> Tiff
getTiff = \case
  Raw _ img -> img
  Master _ img -> img
  Clean _ img -> img
  StarsLocated _ _ img -> img
  Aligned _ img -> img

getName :: Image a -> String
getName = \case
  Raw name _ -> name
  Master name _ -> name
  Clean name _ -> name
  StarsLocated name _ _ -> name
  Aligned name _ -> name

locateStars :: Members '[Cache.Cache StarLocationKey (Image StarsLocated)] r => Image Clean -> Sem r (Image StarsLocated)
locateStars (Clean name img) =
  let yImg@P.Image {..} = flattenImage $ P.extractLumaPlane img
      maxBrightness = VS.maximum imageData
      minBrightness = VS.minimum imageData
      brightnessThreshold = 0.01
      minBrightnessForStar = maxBrightness - round @Double (brightnessThreshold * fromIntegral (maxBrightness - minBrightness))
      starLocations =
        P.pixelFold
          ( \acc x y pixel ->
              if pixel >= minBrightnessForStar
                then
                  let newStarLoc = StarLocation {starLocationX = x, starLocationY = y}
                   in newStarLoc : acc
                else acc
          )
          []
          yImg
   in do
        let startLocationKey = StartLocationKey name
        mLocated <- Cache.get startLocationKey
        case mLocated of
          Nothing -> do
            let located = StarsLocated name starLocations $ drawStarLocations starLocations img
            Cache.update startLocationKey located
            pure located
          Just located -> pure located

drawStarLocations :: [StarLocation] -> Tiff -> Tiff
drawStarLocations starLocations =
  P.pixelMapXY
    ( \pX pY p ->
        let isOnAnyCross xy = any (isOnCross xy) starLocations
            isOnCross (x, y) StarLocation {..} =
              ( abs (x - starLocationX) <= crossSize
                  && abs (y - starLocationY) <= crossThickness
              )
                || ( abs (y - starLocationY) <= crossSize
                       && abs (x - starLocationX) <= crossThickness
                   )
            crossSize = 60
            crossThickness = 2
            crossColor = P.PixelRGB16 maxBound maxBound maxBound
         in if isOnAnyCross (pX, pY)
              then crossColor
              else p
    )

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

-- denoise :: Image Raw -> Image Clean
-- denoise (Raw name img) =
--   let width = Graphics.rows img
--       height = Graphics.cols img
--       blurSize = min width height `div` 4
--       clean = Graphics.applyFilter (Graphics.gaussianLowPass blurSize 10 Graphics.Edge) img
--    in Clean name $ zipWithImages (-) img clean

loadTiff :: FilePath -> IO (Image Raw)
loadTiff imagePath = do
  res <- P.readTiff imagePath
  case res of
    Right (P.ImageRGB16 img) ->
      let name = dropExtensions $ takeFileName imagePath
       in pure $ Raw name img
    Right _ ->
      fail "Incorrect image format"
    Left err -> error err

zipWithImages ::
  ( Vector.Storable (P.PixelBaseComponent a1),
    Vector.Storable (P.PixelBaseComponent a2),
    Vector.Storable (P.PixelBaseComponent a3)
  ) =>
  ( P.PixelBaseComponent a1 ->
    P.PixelBaseComponent a2 ->
    P.PixelBaseComponent a3
  ) ->
  P.Image a1 ->
  P.Image a2 ->
  P.Image a3
zipWithImages f img1 img2 =
  P.Image
    { P.imageHeight = P.imageHeight img1,
      P.imageWidth = P.imageWidth img1,
      P.imageData = VS.zipWith f (P.imageData img1) (P.imageData img2)
    }

generateMaster :: [Image Raw] -> Tiff
generateMaster is =
  foldl1' (zipWithImages (+)) $ map (imageMap (`div` nImages) . getTiff) is
  where
    nImages = fromIntegral (length is)

locateFiles :: FilePath -> IO [FilePath]
locateFiles path =
  map (path <>) . filter (isExtensionOf "tiff") <$> listDirectory path

newtype WorkingDir = WorkingDir {getWorkingDir :: FilePath}

inWorkingDir :: Members '[Reader.Reader WorkingDir] r => FilePath -> Sem r FilePath
inWorkingDir path =
  Reader.asks ((<> "/" <> path) . getWorkingDir)

data MasterKey = MasterKey
  { masterImages :: [FilePath],
    masterTiffPath :: FilePath
  }
  deriving (Show)

imageMap ::
  ( Vector.Storable (P.PixelBaseComponent a1),
    Vector.Storable (P.PixelBaseComponent a2)
  ) =>
  (P.PixelBaseComponent a1 -> P.PixelBaseComponent a2) ->
  P.Image a1 ->
  P.Image a2
imageMap f P.Image {..} =
  P.Image
    { P.imageWidth = imageWidth,
      P.imageHeight = imageHeight,
      P.imageData = VS.map f imageData
    }

runMasterCache :: Members '[Embed IO, Log.Log, Reader.Reader WorkingDir] r => Sem (Cache.Cache MasterKey (Image Master) : r) a -> Sem r a
runMasterCache = interpret $ \case
  Cache.Get k@MasterKey {..} -> do
    path <- inWorkingDir masterTiffPath
    present <- embed $ doesFileExist path
    if present
      then do
        Log.logDebug $ "Cache hit for: " <> show k
        let masterName = dropExtensions $ takeFileName path
        Just . Master masterName . getTiff <$> embed (loadTiff path)
      else do
        Log.logDebug $ "Cache miss for: " <> show k
        pure Nothing
  Cache.Update k@MasterKey {..} v -> do
    Log.logDebug $ "Updated cache entry for " <> show k
    path <- inWorkingDir masterTiffPath
    embed $ P.writeTiff path $ getTiff v

runStarsLocatedCache :: Members '[Embed IO, Log.Log, Reader.Reader WorkingDir] r => Sem (Cache.Cache StarLocationKey (Image StarsLocated) : r) a -> Sem r a
runStarsLocatedCache = interpret $ \case
  Cache.Get k@(StartLocationKey name) -> do
    path <- inWorkingDir name
    starLocationFile <- inWorkingDir (name <> "_star_locations.txt")
    present <- embed $ doesFileExist starLocationFile
    if present
      then do
        Log.logDebug $ "Cache hit for: " <> show k
        starLocations <- map (uncurry StarLocation . read) . lines <$> embed (readFile' starLocationFile)
        Just . StarsLocated name starLocations . getTiff <$> embed (loadTiff path)
      else do
        Log.logDebug $ "Cache miss for: " <> show k
        pure Nothing
  Cache.Update k@(StartLocationKey name) (StarsLocated _ starLocations img) -> do
    Log.logDebug $ "Updated cache entry for " <> show k
    path <- inWorkingDir (name <> "_stars.tiff")
    embed $ P.writeTiff path img
    starLocationFile <- inWorkingDir (name <> "_star_locations.txt")
    embed $ writeFile starLocationFile $ unlines $ map (\StarLocation {..} -> show (starLocationX, starLocationY)) starLocations

runCleanCache :: Members '[Embed IO, Log.Log, Reader.Reader WorkingDir] r => Sem (Cache.Cache CleanKey (Image Clean) : r) a -> Sem r a
runCleanCache = interpret $ \case
  Cache.Get k@(CleanKey name) -> do
    path <- inWorkingDir (name <> "_clean.tiff")
    present <- embed $ doesFileExist path
    if present
      then do
        Log.logDebug $ "Cache hit for: " <> show k
        Just . Clean name . getTiff <$> embed (loadTiff path)
      else do
        Log.logDebug $ "Cache miss for: " <> show k
        pure Nothing
  Cache.Update k@(CleanKey name) v -> do
    Log.logDebug $ "Updated cache entry for " <> show k
    path <- inWorkingDir (name <> "_clean.tiff")
    embed $ P.writeTiff path $ getTiff v

calculateMaster :: Members '[Embed IO, Cache.Cache MasterKey (Image Master)] r => FilePath -> [FilePath] -> Sem r (Image Master)
calculateMaster masterPath origs = do
  let cacheKey = MasterKey origs masterPath
  mCachedMaster <- Cache.get cacheKey
  case mCachedMaster of
    Just cachedMaster -> do
      pure cachedMaster
    Nothing -> do
      master <- generateMaster <$> mapM (embed . loadTiff) origs
      let masterName = dropExtensions $ takeFileName masterPath
          cacheMaster = Master masterName master
      Cache.update cacheKey cacheMaster
      pure cacheMaster

data OuterCorners = OuterCorners
  { upperLeftCorner :: (Int, Int),
    lowerRightCorner :: (Int, Int)
  }
  deriving (Show)

stars540 :: [StarLocation]
stars540 = map (uncurry StarLocation) [(644, 1393), (2764, 4991), (3389, 454)]

alignment540 :: Alignment
alignment540 = Alignment 0 0 0

stars541 :: [StarLocation]
stars541 = map (uncurry StarLocation) [(645, 1385), (2768, 4987), (3392, 447)]

alignment541 :: Alignment
alignment541 = computeAlignment stars540 stars541

computeAlignment :: [StarLocation] -> [StarLocation] -> Alignment
computeAlignment sts1 sts2 = minimumBy (compare `on` alignmentError) [Alignment offX offY rot | rot <- [-1, -0.9 .. 1], offX <- [-20, -19 .. 20], offY <- [-20, -19 .. 20]]
  where
    alignmentError :: Alignment -> Int
    alignmentError alg =
      sum $
        zipWith starDistance (sort sts1) $
          map (\(StarLocation x y) -> uncurry StarLocation $ applyAlignment alg (x, y)) $
            sort sts2

testImgs :: IO [(Tiff, Alignment)]
testImgs = do
  img540 <- drawStarLocations stars540 . getTiff <$> loadTiff "./resources/lights/DSC00540.tiff"
  img541 <- drawStarLocations stars541 . getTiff <$> loadTiff "./resources/lights/DSC00541.tiff"
  pure
    [ (img540, alignment540),
      (img541, traceShowId alignment541)
    ]

-- stars1 = [StarLocation 10 10, StarLocation 10 30, StarLocation 20 80]

-- stars2 = [StarLocation 15 15, StarLocation 15 35, StarLocation 25 85]

-- testImgs =
--   [ ( P.generateImage (\x y -> if StarLocation x y `elem` stars1 then P.PixelRGB16 maxBound maxBound maxBound else PixelRGB16 0 0 0) 40 100,
--       Alignment 0 0 0
--     ),
--     ( P.generateImage (\x y -> if StarLocation x y `elem` stars2 then P.PixelRGB16 maxBound maxBound maxBound else PixelRGB16 0 0 0) 40 100,
--       Alignment (-5) (-5) 0
--     )
--   ]

test = testImgs >>= P.writeTiff "./resources/tmp/stack_correct.tiff" . stackImages

starDistance :: StarLocation -> StarLocation -> Int
starDistance (StarLocation x1 y1) (StarLocation x2 y2) =
  (x1 - x2) ^ 2 + (y1 - y2) ^ 2

applyAlignment :: Alignment -> (Int, Int) -> (Int, Int)
applyAlignment Alignment {..} = translate . rotate rotation
  where
    translate :: (Int, Int) -> (Int, Int)
    translate (x, y) = (x + offsetX, y + offsetY)

rotate :: Double -> (Int, Int) -> (Int, Int)
rotate rotation (x, y) =
  let x' = fromIntegral x
      y' = fromIntegral y
   in (round $ x' * cos rotation - y' * sin rotation, round $ y' * cos rotation + x' * sin rotation)

stackImages :: [(Tiff, Alignment)] -> P.Image P.PixelRGBA16
stackImages [] = error "cannot stack empty set of images"
stackImages images@((P.Image w h _, Alignment offX offY rot) : sources) =
  P.generateImage generatePixel outputWidth outputHeight
  where
    generatePixel x y = avg $ map (lookupPixel upperLeft x y) images

    (outputWidth, outputHeight, upperLeft) =
      outerCornersToDimensions
        $ foldl'
          (\cor (P.Image {..}, alg) -> updateCorners cor imageHeight imageWidth alg)
          (OuterCorners (0, 0) (w, h))
        $ map (fmap ajustAlignment) sources

    ajustAlignment Alignment {..} =
      Alignment
        { offsetX = offsetX - offX,
          offsetY = offsetY - offY,
          rotation = rotation - rot
        }

    outerCornersToDimensions
      OuterCorners
        { upperLeftCorner = ul@(ulX, ulY),
          lowerRightCorner = (lrX, lrY)
        } =
        (lrX - ulX, lrY - ulY, ul)

    updateCorners :: OuterCorners -> Int -> Int -> Alignment -> OuterCorners
    updateCorners cor height width alg =
      let imageCorners = [(0, 0), (0, height), (width, 0), (width, height)]

          alignedImageCorners = map (applyAlignment alg) imageCorners
       in foldl' applyCorner cor alignedImageCorners

    applyCorner :: OuterCorners -> (Int, Int) -> OuterCorners
    applyCorner
      OuterCorners
        { upperLeftCorner = (ulX, ulY),
          lowerRightCorner = (lrX, lrY)
        }
      (x, y) =
        OuterCorners
          { upperLeftCorner = (min ulX x, min ulY y),
            lowerRightCorner = (max lrX x, max lrY y)
          }

    lookupPixel :: (Int, Int) -> Int -> Int -> (Tiff, Alignment) -> Maybe P.PixelRGB16
    lookupPixel (ulX, ulY) outX outY (img@P.Image {..}, Alignment offX offY rot) =
      let (x, y) = rotate (negate rot) (outX + ulX - offX, outY + ulY - offY)
       in if x >= 0
            && x < imageWidth
            && y >= 0
            && y < imageHeight
            then Just $ P.pixelAt img x y
            else Nothing

avg :: [Maybe P.PixelRGB16] -> P.PixelRGBA16
avg xs =
  \case
    [] -> P.PixelRGBA16 0 0 0 0
    ys -> foldl1' (P.mixWith (const (+))) ys
    . mapMaybe
      ( fmap
          ( \(P.PixelRGB16 r g b) ->
              P.PixelRGBA16
                (r `div` n)
                (g `div` n)
                (b `div` n)
                maxBound
          )
      )
    $ xs
  where
    n = fromIntegral $ length $ catMaybes xs

withTiming :: Members '[Time.Time, Log.Log] r => String -> Sem r a -> Sem r a
withTiming actName act = do
  start <- Time.getTime
  res <- act
  end <- Time.getTime
  Log.logInfo $ "Timing (" <> actName <> "): " <> show (diffUTCTime end start)
  pure res

logConfig :: Log.Severity -> Log.LogConfig
logConfig severity =
  ( Log.defaultLogConfig
      `Log.addContext` "astro-tracker"
  )
    { Log.logConfigMinimumSeverity = severity
    }

newtype CleanKey = CleanKey {getCleanKey :: String}
  deriving (Show)

cleanImage :: Members '[Embed IO, Cache.Cache CleanKey (Image Clean), Time.Time, Log.Log] r => Image Master -> Image Master -> (FilePath -> IO (Image Raw)) -> FilePath -> Sem r (Image Clean)
cleanImage (Master _ masterDark) (Master _ masterBias) loadSourceImg imagePath = do
  let cleanKeyName = dropExtensions (takeFileName imagePath)
      cleanKey = CleanKey cleanKeyName
  mClean <- Cache.get cleanKey
  let cleaningAction imgName =
        pure . Clean imgName . subtractMaster masterBias . subtractMaster masterDark
  case mClean of
    Nothing -> do
      sourceImg <- embed $ loadSourceImg imagePath
      case sourceImg of
        Raw imageName lightImg -> do
          clean <- withTiming "Clean " $ cleaningAction imageName lightImg
          Cache.update cleanKey clean
          pure clean
    Just clean -> pure clean
  where
    subtractMaster :: (Vector.Storable (P.PixelBaseComponent a), Num (P.PixelBaseComponent a)) => P.Image a -> P.Image a -> P.Image a
    subtractMaster master img =
      zipWithImages (-) img master

newtype StarLocationKey = StartLocationKey {getLocationKey :: String}
  deriving (Show)

run :: Log.Severity -> [FilePath] -> [FilePath] -> [FilePath] -> FilePath -> IO ()
run logSeverity darks biass lights workingDir = test

--  runM $
-- Reader.runReader (WorkingDir workingDir) $
--   Time.runTime $
--     Log.runLoggingWithTime (logConfig logSeverity) $
--       runStarsLocatedCache $
--         runMasterCache $ do
--           Log.logInfo $ "Darks: " <> show (length darks)
--           Log.logInfo $ "Biass: " <> show (length biass)
--           Log.logInfo $ "Lights: " <> show (length lights)
--           Log.logInfo $ "Working directory:  " <> workingDir
--           (masterDark, masterBias) <-
--             embed $
--               runM
--                 ( Reader.runReader (WorkingDir workingDir) $
--                     Time.runTime $
--                       Log.runLoggingWithTime (logConfig logSeverity) $
--                         withTiming "MasterDark" $
--                           runMasterCache $
--                             calculateMaster "master_dark.tiff" darks
--                 )
--                 `Async.concurrently` runM
--                   ( Reader.runReader (WorkingDir workingDir) $
--                       Time.runTime $
--                         Log.runLoggingWithTime (logConfig logSeverity) $
--                           withTiming "MasterBias" $
--                             runMasterCache $
--                               calculateMaster "master_bias.tiff" biass
--                   )

--           cleanImages <-
--             embed $
--               Async.mapConcurrently
--                 ( runM
--                     . Time.runTime
--                     . Reader.runReader (WorkingDir workingDir)
--                     . Log.runLoggingWithTime (logConfig logSeverity)
--                     . runCleanCache
--                     . cleanImage masterDark masterBias loadTiff
--                 )
--                 lights

--           embed $
--             Async.mapConcurrently_
--               ( runM
--                   . Time.runTime
--                   . Reader.runReader (WorkingDir workingDir)
--                   . Log.runLoggingWithTime (logConfig logSeverity)
--                   . runStarsLocatedCache
--                   . locateStars
--               )
--               cleanImages

---------------------------------------------------------------
-- Pipeline
---------------------------------------------------------------

-- lights:
-- - subtract master dark/bias?
-- - locate stars in image
-- - calculate alignment
-- - stack
-- bias:
-- - generate master bias
-- darks:
-- - generate master dark