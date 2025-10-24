{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module ImageUtils where

import Codec.Picture qualified as P
import Control.Monad.ST
import Data.Maybe
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as MV

{-# SPECIALIZE INLINE pixelZipWith :: (P.PixelYCbCr8 -> P.PixelRGB8 -> P.PixelRGB8) -> P.Image P.PixelYCbCr8 -> P.Image P.PixelRGB8 -> P.Image P.PixelRGB8 #-}
{-# SPECIALIZE INLINE pixelZipWith :: (P.PixelRGB8 -> P.PixelYCbCr8 -> P.PixelYCbCr8) -> P.Image P.PixelRGB8 -> P.Image P.PixelYCbCr8 -> P.Image P.PixelYCbCr8 #-}
{-# SPECIALIZE INLINE pixelZipWith :: (P.PixelRGB8 -> P.PixelRGB8 -> P.PixelRGB8) -> P.Image P.PixelRGB8 -> P.Image P.PixelRGB8 -> P.Image P.PixelRGB8 #-}
{-# SPECIALIZE INLINE pixelZipWith :: (P.PixelRGB8 -> P.PixelRGBA8 -> P.PixelRGBA8) -> P.Image P.PixelRGB8 -> P.Image P.PixelRGBA8 -> P.Image P.PixelRGBA8 #-}
{-# SPECIALIZE INLINE pixelZipWith :: (P.PixelRGBA8 -> P.PixelRGBA8 -> P.PixelRGBA8) -> P.Image P.PixelRGBA8 -> P.Image P.PixelRGBA8 -> P.Image P.PixelRGBA8 #-}
{-# SPECIALIZE INLINE pixelZipWith :: (P.Pixel8 -> P.PixelRGB8 -> P.PixelRGB8) -> P.Image P.Pixel8 -> P.Image P.PixelRGB8 -> P.Image P.PixelRGB8 #-}
{-# SPECIALIZE INLINE pixelZipWith :: (P.Pixel8 -> P.Pixel8 -> P.Pixel8) -> P.Image P.Pixel8 -> P.Image P.Pixel8 -> P.Image P.Pixel8 #-}
pixelZipWith ::
  forall a b c.
  (P.Pixel a, P.Pixel b, P.Pixel c) =>
  (a -> b -> c) ->
  P.Image a ->
  P.Image b ->
  P.Image c
pixelZipWith f P.Image{imageWidth = w, imageHeight = h, imageData = vec1} P.Image{imageData = vec2} =
  P.Image w h pixels
 where
  source1ComponentCount = P.componentCount (undefined :: a)
  source2ComponentCount = P.componentCount (undefined :: b)
  destComponentCount = P.componentCount (undefined :: c)

  pixels = runST $ do
    newArr <- MV.new (w * h * destComponentCount)
    let lineMapper _ _ _ y | y >= h = return ()
        lineMapper readIdx1Line readIdx2Line writeIdxLine y = colMapper readIdx1Line readIdx2Line writeIdxLine 0
         where
          colMapper readIdx1 readIdx2 writeIdx x
            | x >= w = lineMapper readIdx1 readIdx2 writeIdx $ y + 1
            | otherwise = do
                P.unsafeWritePixel newArr writeIdx $ f (P.unsafePixelAt vec1 readIdx1) (P.unsafePixelAt vec2 readIdx2)
                colMapper
                  (readIdx1 + source1ComponentCount)
                  (readIdx2 + source2ComponentCount)
                  (writeIdx + destComponentCount)
                  (x + 1)
    lineMapper 0 0 0 0
    -- unsafeFreeze avoids making a second copy and it will be
    -- safe because newArray can't be referenced as a mutable array
    -- outside of this where block
    VS.unsafeFreeze newArr

{-# SPECIALIZE INLINE extractSubImage :: Int -> Int -> Int -> Int -> P.Image P.Pixel16 -> P.Image P.Pixel16 #-}
{-# SPECIALIZE INLINE extractSubImage :: Int -> Int -> Int -> Int -> P.Image P.PixelRGB16 -> P.Image P.PixelRGB16 #-}
{-# SPECIALIZE INLINE extractSubImage :: Int -> Int -> Int -> Int -> P.Image P.PixelRGBA16 -> P.Image P.PixelRGBA16 #-}
extractSubImage ::
  forall a.
  (P.Pixel a) =>
  Int ->
  Int ->
  Int ->
  Int ->
  P.Image a ->
  P.Image a
extractSubImage xTopLeft yTopLeft width height P.Image{..} =
  P.Image
    { imageWidth = outWidth
    , imageHeight = outHeight
    , imageData = pixels
    }
 where
  componentCount = P.componentCount (undefined :: a)
  minX = max 0 xTopLeft
  maxX = min (imageWidth - 1) $ xTopLeft + width
  minY = max 0 yTopLeft
  maxY = min (imageHeight - 1) $ yTopLeft + height
  outWidth = maxX - minX
  outHeight = maxY - minY
  missingWidth = imageWidth - outWidth

  startReadIdx = (minY * width + minX) * componentCount

  pixels = runST $ do
    newArr <- MV.new (outWidth * outHeight * componentCount)
    let lineMapper _ _ y | y >= maxY = return ()
        lineMapper readIdxLine writeIdxLine y = colMapper readIdxLine writeIdxLine minX
         where
          colMapper readIdx writeIdx x
            | x >= maxX = lineMapper (readIdx + missingWidth) writeIdx (y + 1)
            | otherwise = do
                P.unsafeWritePixel @a newArr writeIdx $ P.unsafePixelAt imageData readIdx
                colMapper
                  (readIdx + componentCount)
                  (writeIdx + componentCount)
                  (x + 1)
    lineMapper startReadIdx 0 minY
    -- unsafeFreeze avoids making a second copy and it will be
    -- safe because newArray can't be referenced as a mutable array
    -- outside of this where block
    VS.unsafeFreeze newArr

{-# INLINE safePixelAt #-}
safePixelAt :: (P.Pixel a) => P.Image a -> Int -> Int -> Maybe a
safePixelAt img@P.Image{..} x y
  | x >= 0
  , x < imageWidth
  , y >= 0
  , y < imageHeight =
      Just $ P.pixelAt img x y
  | otherwise = Nothing

{-# INLINE pixelAtDefault #-}
pixelAtDefault :: (P.Pixel a) => a -> P.Image a -> Int -> Int -> a
pixelAtDefault def img x y = fromMaybe def $ safePixelAt img x y

data RayStep a
  = RayStep
  { north :: a
  , northEast :: a
  , east :: a
  , southEast :: a
  , south :: a
  , southWest :: a
  , west :: a
  , northWest :: a
  }
  deriving (Show, Functor, Foldable, Eq)

mapRayStep :: ((Int, Int) -> a -> b) -> RayStep a -> RayStep b
mapRayStep f RayStep{..} =
  RayStep
    { north = f (0, -1) north
    , northEast = f (1, -1) northEast
    , east = f (1, 0) east
    , southEast = f (1, 1) southEast
    , south = f (0, 1) south
    , southWest = f (-1, 1) southWest
    , west = f (-1, 0) west
    , northWest = f (-1, -1) northWest
    }

zipWithRayStep :: (a -> b -> c) -> RayStep a -> RayStep b -> RayStep c
zipWithRayStep f rs1 rs2 =
  RayStep
    { north = f rs1.north rs2.north
    , northEast = f rs1.northEast rs2.northEast
    , east = f rs1.east rs2.east
    , southEast = f rs1.southEast rs2.southEast
    , south = f rs1.south rs2.south
    , southWest = f rs1.southWest rs2.southWest
    , west = f rs1.west rs2.west
    , northWest = f rs1.northWest rs2.northWest
    }

rayDirs :: RayStep (Int, Int)
rayDirs =
  RayStep
    { north = (0, -1)
    , northEast = (1, -1)
    , east = (1, 0)
    , southEast = (1, 1)
    , south = (0, 1)
    , southWest = (-1, 1)
    , west = (-1, 0)
    , northWest = (-1, -1)
    }

both :: (t -> b) -> (t, t) -> (b, b)
both f (x, y) = (f x, f y)

elementWise :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
elementWise f (x, y) (w, v) = (f x w, f y v)

generateRays :: (P.Pixel a, Num a) => Int -> Int -> Int -> P.Image a -> [RayStep a]
generateRays x y rayLength img = go 1 []
 where
  go !l !rays
    | l > rayLength = reverse rays
    | otherwise =
        let nextRayStepM = fmap (uncurry (safePixelAt img) . elementWise (+) (x, y) . both (* l)) rayDirs
         in case nextRayStepM of
              RayStep
                { north = Nothing
                , northEast = Nothing
                , east = Nothing
                , southEast = Nothing
                , south = Nothing
                , southWest = Nothing
                , west = Nothing
                , northWest = Nothing
                } -> reverse rays
              nextRayStep -> go (l + 1) (fmap (fromMaybe 0) nextRayStep : rays)
