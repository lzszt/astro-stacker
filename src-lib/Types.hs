{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Codec.Picture qualified as P

type Tiff = P.Image P.PixelRGB16

data Alignment = Alignment
  { offsetX :: Int,
    offsetY :: Int,
    rotation :: Double
  }
  deriving (Show)

rotate :: Double -> Position -> Position
rotate 0 p = p
rotate rotation Position {..} =
  let x' = fromIntegral x
      y' = fromIntegral y
   in Position
        (round $ x' * cos rotation - y' * sin rotation)
        (round $ y' * cos rotation + x' * sin rotation)

applyAlignment :: Alignment -> Position -> Position
applyAlignment Alignment {..}
  | rotation == 0 = translate
  | otherwise = translate . rotate rotation
  where
    translate :: Position -> Position
    translate Position {..} = Position (x + offsetX) (y + offsetY)

data OuterCorners = OuterCorners
  { upperLeftCorner :: (Int, Int),
    lowerRightCorner :: (Int, Int)
  }
  deriving (Show)

data Position = Position
  { x :: Int,
    y :: Int
  }
  deriving (Show, Read, Eq, Ord)

class Located a where
  position :: a -> Position

instance Located Position where
  position = id

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
  -- deriving (Show, Read, Eq, Ord)
  deriving (Read, Eq, Ord)

instance Show Star where
  show (Star (Position x y) r) = "(" <> show x <> "," <> show y <> ")*" <> show r

instance Located Star where
  position = starPosition

newtype RefStar = RefStar {unRef :: Star}
  deriving (Eq, Ord, Show, Located, IsStar)

newtype TargetStar = TargetStar {unTarget :: Star}
  deriving (Show, Located, Eq, Ord, IsStar)

locatedDistance :: (Located a, Located b) => a -> b -> Double
locatedDistance x y = distance (position x) (position y)

class (Located s) => IsStar s where
  toStar :: s -> Star

instance IsStar Star where
  toStar = id

starDistance :: Star -> Star -> Double
starDistance s1 s2 = distance s1.starPosition s2.starPosition

data Image = Image
  { image :: Tiff,
    imageName :: String
  }

instance Show Image where
  show i = i.imageName <> " " <> show i.image.imageWidth <> "x" <> show i.image.imageHeight

data AlignmentMode
  = NoAlignment
  | TranslationOnly
  | TranslateAndRotate
  deriving (Show)

newtype Unordered a = Unordered {getUnordered :: (a, a)}
  deriving (Show)

mkUnordered :: (Ord a) => a -> a -> Unordered a
mkUnordered x y
  | x <= y = Unordered (x, y)
  | otherwise = Unordered (y, x)

instance (Eq a) => Eq (Unordered a) where
  Unordered p1 == Unordered p2 = p1 == p2

instance (Ord a) => Ord (Unordered a) where
  compare (Unordered p1) (Unordered p2) = compare p1 p2