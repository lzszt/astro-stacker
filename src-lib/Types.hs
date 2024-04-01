module Types
  ( Tiff,
    Alignment (..),
    OuterCorners (..),
    StarLocation (..),
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

data StarLocation = StarLocation
  { starLocationX :: Int,
    starLocationY :: Int
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
  StarsLocated :: String -> [StarLocation] -> Tiff -> Image StarsLocated
  Aligned :: String -> Tiff -> Image Aligned