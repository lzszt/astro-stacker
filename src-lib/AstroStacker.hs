{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AstroStacker where

import Codec.Picture qualified as P
import Config
import Data.List
import Data.Maybe
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable qualified as Vector
import Polysemy
import Polysemy.Reader qualified as Reader
import System.Directory
import System.FilePath
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

locateTiffs :: FilePath -> IO [FilePath]
locateTiffs path =
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

runStacking :: Config -> IO ()
runStacking conf = do
  putStrLn $ "Working directory: " <> conf.workingDirectory
  lightFrames <- locateTiffs conf.lightFramesDirectory
  darkFrames <- fromMaybe [] <$> mapM locateTiffs conf.darkFramesDirectory
  biasFrames <- fromMaybe [] <$> mapM locateTiffs conf.biasFramesDirectory
  flatFrames <- fromMaybe [] <$> mapM locateTiffs conf.flatFramesDirectory
  putStrLn $ "Found " <> show (length lightFrames) <> " light frames"
  putStrLn $ "Found " <> show (length darkFrames) <> " dark frames"
  putStrLn $ "Found " <> show (length biasFrames) <> " bias frames"
  putStrLn $ "Found " <> show (length flatFrames) <> " flat frames"