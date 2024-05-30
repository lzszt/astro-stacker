{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AstroStacker where

import Align
import Codec.Picture qualified as P
import Codec.Picture.Types qualified as P
import Config
import Control.Concurrent.Async qualified as Async
import Data.Bifunctor
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe
import ImageUtils
import Locating
import Stacking
import System.Directory
import System.FilePath
import Types

loadImage :: FilePath -> IO Image
loadImage imagePath = do
  res <- P.readTiff imagePath
  case res of
    Right (P.ImageRGB16 img) ->
      let name = dropExtensions $ takeFileName imagePath
       in pure $ Image img name
    Right _ ->
      fail "Incorrect image format"
    Left err -> error err

saveImage :: FilePath -> Image -> IO ()
saveImage workingDir Image {..} =
  P.writeTiff (workingDir </> imageName <.> "tiff") image

generateMaster :: String -> [Tiff] -> Image
generateMaster label is =
  let masterTiff =
        foldl1' (pixelZipWith addPixels) $
          map
            ( P.pixelMap
                ( \(P.PixelRGB16 r g b) ->
                    P.PixelRGB16
                      (r `div` nImages)
                      (g `div` nImages)
                      (b `div` nImages)
                )
            )
            is
   in Image masterTiff (label <> "_master")
  where
    addPixels (P.PixelRGB16 r1 b1 g1) (P.PixelRGB16 r2 b2 g2) =
      P.PixelRGB16
        (r1 + r2)
        (b1 + b2)
        (g1 + g2)
    nImages = fromIntegral (length is)

calculateMaster :: FilePath -> String -> [FilePath] -> IO (Maybe Image)
calculateMaster _ _ [] = pure Nothing
calculateMaster workingDir label paths = do
  tiffs <- mapM (fmap (.image) . loadImage) paths
  let master@Image {..} = generateMaster label tiffs
  saveImage workingDir master
  pure $ Just master

subtractMasterDark :: Tiff -> Tiff -> Tiff
subtractMasterDark masterDark lightTiff =
  pixelZipWith
    ( \(P.PixelRGB16 lr lg lb) (P.PixelRGB16 dr dg db) ->
        P.PixelRGB16
          (lr - dr)
          (lg - dg)
          (lb - db)
    )
    lightTiff
    masterDark

applyMasterDark :: FilePath -> Image -> FilePath -> IO Image
applyMasterDark workingDir masterDark lightFramePath = do
  Image {..} <- loadImage lightFramePath
  let cleanTiff = subtractMasterDark masterDark.image image
      cleanImage = Image cleanTiff (imageName <> "_clean")
  saveImage workingDir cleanImage
  pure cleanImage

locateStars :: FilePath -> Image -> IO (Maybe (Image, [Star]))
locateStars workingDir lightFrame@Image {..} = do
  let stars = locateStarsDSS $ P.extractLumaPlane image
  putStrLn $ "Found " <> show (length stars) <> " stars in " <> show imageName
  writeFile (workingDir </> imageName <> "_stars.txt") $ show stars
  pure $ Just (lightFrame, stars)

locateTiffs :: FilePath -> IO [FilePath]
locateTiffs path =
  map (path <>) . filter (isExtensionOf "tiff") <$> listDirectory path

calculateAlignment :: FilePath -> String -> [Star] -> [Star] -> IO (Maybe Alignment)
calculateAlignment workingDir imageName refStars targetStars = do
  if length refStars < 3 || length targetStars < 3
    then do
      putStrLn $ "Cannot compute alignment from fewer than 3 stars"
      pure Nothing
    else do
      let Just res = computeLargeTriangleTransformation refStars targetStars
      print $ length res
      let uniques = M.toList $ M.fromListWith const res
      print $ length uniques
      let alignment = Alignment 0 0 0
      writeFile (workingDir </> imageName <> "_alignment.txt") $ show alignment
      pure $ Just alignment

performStacking :: FilePath -> [(Image, Alignment)] -> IO ()
performStacking workingDir lightFrames = do
  let lightTiffs = map (first (.image)) lightFrames
  let stackedTiff = stackImages lightTiffs
  P.writeTiff (workingDir </> "stacked" <.> "tiff") stackedTiff

newtype WorkingDir = WorkingDir {getWorkingDir :: FilePath}

inWorkingDir :: WorkingDir -> FilePath -> FilePath
inWorkingDir wd path =
  wd.getWorkingDir </> path

runStacking :: Config -> IO ()
runStacking conf = do
  putStrLn $ "Working directory: " <> conf.workingDirectory
  lightFramePaths <- locateTiffs conf.lightFramesDirectory
  darkFramePaths <- fromMaybe [] <$> mapM locateTiffs conf.darkFramesDirectory
  biasFramePaths <- fromMaybe [] <$> mapM locateTiffs conf.biasFramesDirectory
  flatFramePaths <- fromMaybe [] <$> mapM locateTiffs conf.flatFramesDirectory
  putStrLn $ "Found " <> show (length lightFramePaths) <> " light frames"
  putStrLn $ "Found " <> show (length darkFramePaths) <> " dark frames"
  putStrLn $ "Found " <> show (length biasFramePaths) <> " bias frames"
  putStrLn $ "Found " <> show (length flatFramePaths) <> " flat frames"

  [mMasterDark, _mMasterBias, _mMasterFlat] <-
    Async.mapConcurrently (uncurry (calculateMaster conf.workingDirectory)) [("dark", darkFramePaths), ("bias", biasFramePaths), ("flat", flatFramePaths)]

  cleanLightFrames <-
    case mMasterDark of
      Nothing -> Async.mapConcurrently loadImage lightFramePaths
      Just masterDark ->
        Async.mapConcurrently (applyMasterDark conf.workingDirectory masterDark) lightFramePaths

  ((referenceFrame, referenceStars) : restStackableFramesWithStars) <- catMaybes <$> Async.mapConcurrently (locateStars conf.workingDirectory) cleanLightFrames

  alignedFrames <-
    ((referenceFrame, Alignment 0 0 0) :) . catMaybes
      <$> Async.mapConcurrently
        ( \(lightFrame, frameStars) ->
            fmap (lightFrame,)
              <$> calculateAlignment conf.workingDirectory lightFrame.imageName referenceStars frameStars
        )
        restStackableFramesWithStars

  performStacking conf.workingDirectory alignedFrames