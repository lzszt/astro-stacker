{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Align
import Data.Function
import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Types

main :: IO ()
main = hspec $ do
  spec

newtype StarList = StarList [Star]
  deriving (Show)

instance Arbitrary StarList where
  arbitrary = do
    size <- min 20 . (+ 4) . getPositive <$> arbitrary

    StarList . take size . nubBy ((==) `on` starPosition) . getInfiniteList <$> arbitrary

-- shrink (StarList xs) =
--   let shrunkenLength = take 5 $ map (\n -> StarList $ take n xs) $ shrink (length xs)
--       shrunkenElement = take 5 $ map StarList $ filter ((>= 4) . length) $ map (nubBy ((==) `on` starPosition)) $ map shrink xs
--    in shrunkenLength <> shrunkenElement

instance Arbitrary Position where
  arbitrary = do
    x <- getNonNegative <$> arbitrary
    y <- getNonNegative <$> arbitrary
    pure $ Position {x = x, y = y}

-- shrink (Position x y) =
--   (flip Position y <$> shrink x)
--     <> (Position x <$> shrink y)

instance Arbitrary Star where
  arbitrary = do
    pos <- arbitrary
    size <- getPositive <$> arbitrary
    pure $ Star pos size

-- shrink (Star position size) = flip Star size <$> shrink position

applyOffset :: (Int, Int) -> Star -> Star
applyOffset (offX, offY) s = s {starPosition = translate s.starPosition}
  where
    translate (Position x y) = Position (x + offX) (y + offY)

spec :: Spec
spec = do
  describe "foo" $
    it "bar" foo
  describe "resolveVotes" $
    it "should never return multiple entries for the same element" bar

-- describe "addition" $ do
--   prop "should be comutative" $ \(NonEmpty sts) (Positive offX, Positive offY) ->
--     let refStars = map RefStar sts
--         tgtStars = map (TargetStar . applyOffset (offX, offY)) sts
--      in -- tgtStars = map TargetStar sts
--         computeLargeTriangleTransformation refStars tgtStars `shouldBe` Just (zip refStars tgtStars)

---------------------------------------------------------------
-- Expectations for computeLargeTriangleTransformation
---------------------------------------------------------------

-- if refs == tgts then it should find all pairs
-- output should never contain duplicate refs or tgts

foo :: Expectation
foo =
  let stars =
        [ Star {starPosition = Position {x = 10, y = 10}, starRadius = 1}, -- 1
          Star {starPosition = Position {x = 100, y = 200}, starRadius = 2}, -- 2
          Star {starPosition = Position {x = 300, y = 50}, starRadius = 3} -- 3
        ]

      -- d12 = 44200
      -- d13 = 85700
      -- d23 = 62500
      refStars = stars
      tgtStars = stars
      expected = sort $ zip refStars tgtStars
   in case computeLargeTriangleTransformation refStars tgtStars of
        Just actual -> sort actual `shouldStartWith` expected
        Nothing -> expectationFailure "No matching stars found"

bar :: Expectation
bar =
  let inp =
        [ ((Star (Position 10 10) 1, Star (Position 10 10) 1), 1),
          ((Star (Position 100 200) 2, Star (Position 100 200) 2), 1),
          ((Star (Position 300 50) 3, Star (Position 300 50) 3), 1),
          ((Star (Position 10 10) 1, Star (Position 100 200) 2), 0),
          ((Star (Position 10 10) 1, Star (Position 300 50) 3), 0),
          ((Star (Position 100 200) 2, Star (Position 10 10) 1), 0),
          ((Star (Position 100 200) 2, Star (Position 300 50) 3), 0),
          ((Star (Position 300 50) 3, Star (Position 10 10) 1), 0),
          ((Star (Position 300 50) 3, Star (Position 100 200) 2), 0)
        ]
      expected =
        [ (Star (Position 10 10) 1, Star (Position 10 10) 1),
          (Star (Position 100 200) 2, Star (Position 100 200) 2),
          (Star (Position 300 50) 3, Star (Position 300 50) 3)
        ]
   in resolveVotes 3 inp `shouldBe` expected