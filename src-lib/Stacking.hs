{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Stacking
  ( stackImages,
    starLocationApplyAlignment,
  )
where

import Codec.Picture qualified as P
import Data.List
import Data.Maybe
import Types

rotate :: Double -> (Int, Int) -> (Int, Int)
rotate rotation (x, y) =
  let x' = fromIntegral x
      y' = fromIntegral y
   in (round $ x' * cos rotation - y' * sin rotation, round $ y' * cos rotation + x' * sin rotation)

applyAlignment :: Alignment -> (Int, Int) -> (Int, Int)
applyAlignment Alignment {..} = translate . rotate rotation
  where
    translate :: (Int, Int) -> (Int, Int)
    translate (x, y) = (x + offsetX, y + offsetY)

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

starLocationApplyAlignment :: Alignment -> StarLocation -> StarLocation
starLocationApplyAlignment alg (StarLocation x y) =
  uncurry StarLocation $ applyAlignment alg (x, y)

-- testImgs :: IO [(Tiff, Alignment)]
-- testImgs = do
--   img540 <- drawStarLocations stars540 . getTiff <$> loadTiff "./resources/lights/DSC00540.tiff"
--   img541 <- drawStarLocations stars541 . getTiff <$> loadTiff "./resources/lights/DSC00541.tiff"
--   pure
--     [ (img540, alignment540),
--       (img541, traceShowId alignment541)
--     ]

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

-- test = testImgs >>= P.writeTiff "./resources/tmp/stack_correct.tiff" . stackImages

-- stars540 :: [StarLocation]
-- stars540 = map (uncurry StarLocation) [(644, 1393), (2764, 4991), (3389, 454)]

-- alignment540 :: Alignment
-- alignment540 = Alignment 0 0 0

-- stars541 :: [StarLocation]
-- stars541 = map (uncurry StarLocation) [(645, 1385), (2768, 4987), (3392, 447)]

-- alignment541 :: Alignment
-- alignment541 = computeAlignment stars540 stars541
