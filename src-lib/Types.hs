{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Codec.Picture qualified as P

type Tiff = P.Image P.PixelRGB16

data Alignment = Alignment
  { offsetX :: Double,
    offsetY :: Double,
    rotation :: Double
  }
  deriving (Show, Read)

alignmentRef :: Alignment
alignmentRef = Alignment 0 0 0

rotate :: Double -> Position -> Position
rotate 0 p = p
rotate rotation Position {..} =
  Position
    (x * cos rotation - y * sin rotation)
    (y * cos rotation + x * sin rotation)

applyAlignment :: Alignment -> Position -> Position
applyAlignment Alignment {..}
  | rotation == 0 = translate
  | otherwise = translate . rotate rotation
  where
    translate :: Position -> Position
    translate Position {..} = Position (x + offsetX) (y + offsetY)

reverseApplyAlignment :: Alignment -> Position -> Position
reverseApplyAlignment Alignment {..}
  | rotation == 0 = translate
  | otherwise = rotate (-rotation) . translate
  where
    translate :: Position -> Position
    translate Position {..} = Position (x - offsetX) (y - offsetY)

data OuterCorners = OuterCorners
  { upperLeftCorner :: (Int, Int),
    lowerRightCorner :: (Int, Int)
  }
  deriving (Show)

data Position' a = Position
  { x :: a,
    y :: a
  }
  deriving (Show, Read, Eq, Ord, Functor)

type Position = Position' Double

class Located a where
  position :: a -> Position
  updatePosition :: (Position -> Position) -> a -> a

instance Located Position where
  position = id
  updatePosition = id

{-# INLINE distance #-}
distance :: Position -> Position -> Double
distance p1 p2 = sqrt $ distanceSqr p1 p2

{-# INLINE distanceSqr #-}
distanceSqr :: Position -> Position -> Double
distanceSqr p1 p2 =
  (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y)

data Star = Star
  { starPosition :: Position,
    starRadius :: Double
  }
  deriving (Show, Read, Eq, Ord)

instance Located Star where
  position = starPosition
  updatePosition f s = s {starPosition = f s.starPosition}

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