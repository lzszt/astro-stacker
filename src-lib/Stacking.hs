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
      let imageCorners = map (uncurry Position) [(0, 0), (0, fromIntegral height), (fromIntegral width, 0), (fromIntegral width, fromIntegral height)]

          alignedImageCorners = map (applyAlignment alg) imageCorners
       in foldl' applyCorner cor alignedImageCorners

    applyCorner :: OuterCorners -> Position -> OuterCorners
    applyCorner
      OuterCorners
        { upperLeftCorner = (ulX, ulY),
          lowerRightCorner = (lrX, lrY)
        }
      Position {..} =
        OuterCorners
          { upperLeftCorner = (min ulX (floor x), min ulY (floor y)),
            lowerRightCorner = (max lrX (ceiling x), max lrY (ceiling y))
          }

    lookupPixel :: (Int, Int) -> Int -> Int -> (Tiff, Alignment) -> Maybe P.PixelRGB16
    lookupPixel (ulX, ulY) outX outY (img@P.Image {..}, Alignment offX offY rot) =
      let Position {..} = rotate (negate rot) $ Position (fromIntegral (outX + ulX) - offX) (fromIntegral (outY + ulY) - offY)
       in if x >= 0
            && x < fromIntegral imageWidth
            && y >= 0
            && y < fromIntegral imageHeight
            then Just $ P.pixelAt img (round x) (round y)
            else Nothing

starLocationApplyAlignment :: Alignment -> Star -> Star
starLocationApplyAlignment alg (Star pos starRadius) =
  (`Star` starRadius) $ applyAlignment alg pos
