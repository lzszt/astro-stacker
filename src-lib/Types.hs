{-# LANGUAGE OverloadedRecordDot #-}

module Types
  ( Tiff,
    Alignment (..),
    OuterCorners (..),
    Position (..),
    distance,
    Star (..),
    Image (..),
    Raw,
    Master,
    Clean,
    StarsLocated,
    Aligned,
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

distance :: Position -> Position -> Double
distance p1 p2 =
  sqrt $ fromIntegral $ (p1.x - p2.x) ^ 2 + (p1.y - p2.y) ^ 2

data Star = Star
  { starPosition :: Position,
    starRadius :: Double
  }
  deriving (Show, Eq, Ord)

data Raw

data Master

data Clean

data StarsLocated

data Aligned

data Image a where
  Raw :: String -> Tiff -> Image Raw
  Master :: String -> Tiff -> Image Master
  Clean :: String -> Tiff -> Image Clean
  StarsLocated :: String -> [Star] -> Tiff -> Image StarsLocated
  Aligned :: String -> Tiff -> Image Aligned