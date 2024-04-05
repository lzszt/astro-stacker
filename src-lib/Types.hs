{-# LANGUAGE OverloadedRecordDot #-}

module Types
  ( Tiff,
    Alignment (..),
    OuterCorners (..),
    Position (..),
    distance,
    distanceSqr,
    Star (..),
    Image (..),
  )
where

import Codec.Picture qualified as P

type Tiff = P.Image P.PixelRGB16

data Alignment = Alignment
  { offsetX :: Int,
    offsetY :: Int,
    rotation :: Double
  }
  deriving (Show)

data OuterCorners = OuterCorners
  { upperLeftCorner :: (Int, Int),
    lowerRightCorner :: (Int, Int)
  }
  deriving (Show)

data Position = Position
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Ord)

{-# INLINE distance #-}
distance :: Position -> Position -> Double
distance p1 p2 = sqrt $ distanceSqr p1 p2

{-# INLINE distanceSqr #-}
distanceSqr :: Position -> Position -> Double
distanceSqr p1 p2 =
  fromIntegral $ (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y)

data Star = Star
  { starPosition :: Position,
    starRadius :: Double
  }
  deriving (Show, Eq, Ord)

data Image = Image
  { image :: Tiff,
    imageName :: String
  }

instance Show Image where
  show i = i.imageName <> " " <> show i.image.imageWidth <> "x" <> show i.image.imageHeight