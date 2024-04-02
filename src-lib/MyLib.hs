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
import Data.Time (diffUTCTime)
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable qualified as Vector
import Effects.Cache qualified as Cache
import Effects.Logging qualified as Log
import Effects.Time qualified as Time
import Locating qualified
import Polysemy
import Polysemy.Reader qualified as Reader
import Stacking
import System.Directory
import System.FilePath
import System.IO (readFile')
import Types

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

-- locateStars :: Members '[Cache.Cache StarLocationKey (Image StarsLocated)] r => Image Clean -> Sem r (Image StarsLocated)
-- locateStars (Clean name img) = do
--   let startLocationKey = StartLocationKey name
--   mLocated <- Cache.get startLocationKey
--   case mLocated of
--     Nothing -> do
--       let starLocations = Locating.locateStars img
--           located = StarsLocated name starLocations $ Locating.drawStarLocations starLocations img
--       Cache.update startLocationKey located
--       pure located
--     Just located -> pure located

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

-- runStarsLocatedCache :: Members '[Embed IO, Log.Log, Reader.Reader WorkingDir] r => Sem (Cache.Cache StarLocationKey (Image StarsLocated) : r) a -> Sem r a
-- runStarsLocatedCache = interpret $ \case
--   Cache.Get k@(StartLocationKey name) -> do
--     path <- inWorkingDir name
--     starLocationFile <- inWorkingDir (name <> "_star_locations.txt")
--     present <- embed $ doesFileExist starLocationFile
--     if present
--       then do
--         Log.logDebug $ "Cache hit for: " <> show k
--         starLocations <- map (uncurry StarLocation . read) . lines <$> embed (readFile' starLocationFile)
--         Just . StarsLocated name starLocations . getTiff <$> embed (loadTiff path)
--       else do
--         Log.logDebug $ "Cache miss for: " <> show k
--         pure Nothing
--   Cache.Update k@(StartLocationKey name) (StarsLocated _ starLocations img) -> do
--     Log.logDebug $ "Updated cache entry for " <> show k
--     path <- inWorkingDir (name <> "_stars.tiff")
--     embed $ P.writeTiff path img
--     starLocationFile <- inWorkingDir (name <> "_star_locations.txt")
--     embed $ writeFile starLocationFile $ unlines $ map (\(Star ) -> show (starLocationX, starLocationY)) starLocations

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

-- computeAlignment :: [Star] -> [Star] -> Alignment
-- computeAlignment sts1 sts2 = minimumBy (compare `on` alignmentError) [Alignment offX offY rot | rot <- [-1, -0.9 .. 1], offX <- [-20, -19 .. 20], offY <- [-20, -19 .. 20]]
--   where
--     (matchedSts1, matchedSts2) = matchStars (sts1, sts2)
--     alignmentError :: Alignment -> Int
--     alignmentError alg =
--       sum $
--         zipWith starDistance matchedSts1 $
--           map (starLocationApplyAlignment alg) matchedSts2

-- matchStars :: ([Star], [Star]) -> ([Star], [Star])
-- matchStars (x, y) = (sort x, sort y)

-- starDistance :: Star -> Star -> Int
-- starDistance (Star x1 y1) (Star x2 y2) =
--   (x1 - x2) ^ 2 + (y1 - y2) ^ 2

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

-- run :: Log.Severity -> [FilePath] -> [FilePath] -> [FilePath] -> FilePath -> IO ()
-- run logSeverity darks biass lights workingDir =
--   runM $
--     Reader.runReader (WorkingDir workingDir) $
--       Time.runTime $
--         Log.runLoggingWithTime (logConfig logSeverity) $
--           runStarsLocatedCache $
--             runMasterCache $ do
--               Log.logInfo $ "Darks: " <> show (length darks)
--               Log.logInfo $ "Biass: " <> show (length biass)
--               Log.logInfo $ "Lights: " <> show (length lights)
--               Log.logInfo $ "Working directory:  " <> workingDir
--               (masterDark, masterBias) <-
--                 embed $
--                   runM
--                     ( Reader.runReader (WorkingDir workingDir) $
--                         Time.runTime $
--                           Log.runLoggingWithTime (logConfig logSeverity) $
--                             withTiming "MasterDark" $
--                               runMasterCache $
--                                 calculateMaster "master_dark.tiff" darks
--                     )
--                     `Async.concurrently` runM
--                       ( Reader.runReader (WorkingDir workingDir) $
--                           Time.runTime $
--                             Log.runLoggingWithTime (logConfig logSeverity) $
--                               withTiming "MasterBias" $
--                                 runMasterCache $
--                                   calculateMaster "master_bias.tiff" biass
--                       )

--               cleanImages <-
--                 embed $
--                   Async.mapConcurrently
--                     ( runM
--                         . Time.runTime
--                         . Reader.runReader (WorkingDir workingDir)
--                         . Log.runLoggingWithTime (logConfig logSeverity)
--                         . runCleanCache
--                         . cleanImage masterDark masterBias loadTiff
--                     )
--                     lights

--               embed $
--                 Async.mapConcurrently_
--                   ( runM
--                       . Time.runTime
--                       . Reader.runReader (WorkingDir workingDir)
--                       . Log.runLoggingWithTime (logConfig logSeverity)
--                       . runStarsLocatedCache
--                       . locateStars
--                   )
--                   cleanImages

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