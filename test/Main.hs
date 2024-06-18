{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Codec.Picture qualified as P
import Codec.Picture.Types qualified as P
import Data.Function
import Data.List
import ImageUtils
import Locating
import Matching
import System.FilePath
import Test.Hspec
import Test.Hspec.Expectations.Pretty qualified as HP
import Test.Hspec.Golden qualified as G
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Types

main :: IO ()
main = hspec $ do
  -- spec
  alignmentSpec

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

  shrink (Position x y) =
    (flip Position y <$> shrink x)
      <> (Position x <$> shrink y)

instance Arbitrary Star where
  arbitrary = do
    pos <- arbitrary
    size <- getPositive <$> arbitrary
    pure $ Star pos size

-- shrink (Star position size) = flip Star size <$> shrink position

-- applyOffset :: (Int, Int) -> Star -> Star
-- applyOffset (offX, offY) s = s {starPosition = translate s.starPosition}
--   where
--     translate (Position x y) = Position (x + offX) (y + offY)

customGoldenPretty :: (Show str, Read str) => FilePath -> (str -> String) -> str -> G.Golden str
customGoldenPretty name fmt content =
  G.Golden
    { G.output = content,
      G.encodePretty = fmt,
      G.writeToFile = (\path -> writeFile path . show),
      G.readFromFile = (fmap read . readFile),
      G.goldenFile = "test" </> ".golden" </> name </> "golden",
      G.actualFile = Just ("test" </> ".golden" </> name </> "actual"),
      G.failFirstTime = True
    }

customGolden :: FilePath -> String -> G.Golden String
customGolden name = customGoldenPretty name show

formatStar :: Star -> String
formatStar (Star (Position x y) r) = "(" <> show x <> "," <> show y <> ")*" <> show r

formatStars :: [(Star, Star)] -> String
formatStars = unwords . map (\(ref, tgt) -> "(" <> formatStar ref <> "," <> formatStar tgt <> ")")

instance (Arbitrary a) => Arbitrary (RayStep a) where
  arbitrary =
    RayStep
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary PixelDirection' where
  arbitrary =
    PixelDirection'
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

spec :: Spec
spec = do
  describe "foo" $
    it "bar" foo
  describe "resolveVotes" $
    it "should never return multiple entries for the same element" bar
  -- describe "computeLargeTriangleTransformation" $ do
  --   refStars :: [Star] <- runIO $ read <$> readFile "./test/.golden/computeLargeTriangleTransformation/stars1.txt"
  --   targetStars :: [Star] <- runIO $ read <$> readFile "./test/.golden/computeLargeTriangleTransformation/stars2.txt"
  --   -- res <- runIO $ read <$> readFile "./test/.golden/computeLargeTriangleTransformation/golden"
  --   it "should produce the correct star mapping for the sample inputs" $
  --     let result = computeLargeTriangleTransformation refStars targetStars
  --      in --  in result `HP.shouldBe` res

  --         customGoldenPretty "computeLargeTriangleTransformation" formatStars result

  describe "locateStarsDSS" $ do
    img <- runIO $ P.readTiff "./resources/test_lights/img_1.tiff"
    -- res <- runIO $ read <$> readFile "./test/.golden/locateStarsDSS/golden"
    case img of
      Right (P.ImageRGB16 refTiff) ->
        it "should produce correct stars" $
          let refLuma = P.extractLumaPlane refTiff
              stars = locateStarsDSS refLuma
           in customGoldenPretty "locateStarsDSS" show stars
  --  in stars `HP.shouldBe` res
  describe "rayStepFromDirections . directionsFromRayStep" $
    prop "should be the identity function" $ \rayStep ->
      let res = rayStepFromDirections $ directionsFromRayStep rayStep
       in res `shouldBe` rayStep
  describe " directionsFromRayStep . rayStepFromDirections" $
    prop "should be the identity function" $ \rayStep ->
      let directions = directionsFromRayStep rayStep
          res = directionsFromRayStep $ rayStepFromDirections directions
       in res `shouldBe` directions

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
        actual -> sort actual `shouldStartWith` expected

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

newtype Angle = Angle {unAngle :: Double}
  deriving (Show)

instance Arbitrary Angle where
  arbitrary = Angle <$> choose (0, 2 * pi)
  shrink (Angle a) = Angle <$> shrink a

instance Arbitrary Alignment where
  arbitrary =
    Alignment
      <$> arbitrary
      <*> arbitrary
      <*> (unAngle <$> arbitrary)

  shrink (Alignment offX offY rot) =
    (Alignment offX offY . unAngle <$> shrink (Angle rot))
      <> ((\y -> Alignment offX y rot) <$> shrink offY)
      <> ((\x -> Alignment x offY rot) <$> shrink offX)

epsilon :: Double
epsilon = 0.000001

positionsEqual :: Position -> Position -> Bool
positionsEqual (Position x1 y1) (Position x2 y2) =
  abs (x1 - x2) <= epsilon
    && abs (y1 - y2) <= epsilon

alignmentSpec :: Spec
alignmentSpec = do
  describe "applyAlignment alignmentRef" $
    prop "should be the identity" $ \pos ->
      (applyAlignment alignmentRef pos, pos) `shouldSatisfy` uncurry positionsEqual
  describe "rotating by a multiple of 2*pi" $
    prop "should be the identity" $ \(Positive (n :: Int)) pos ->
      (rotate (2 * pi * fromIntegral n) pos, pos) `shouldSatisfy` uncurry positionsEqual
  describe "rotating by pi/2" $
    it "should rotate (1,0) -> (0,1)" $
      (rotate (pi / 2) (Position 1 0), Position 0 1) `shouldSatisfy` uncurry positionsEqual
  describe "rotate" $
    prop "should not change distance from origin" $ \(Angle rot) pos ->
      let distToOrigin = distanceSqr (Position 0 0)
       in (distToOrigin (rotate rot pos), distToOrigin pos) `shouldSatisfy` uncurry (\x y -> abs (x - y) <= epsilon)
  describe "reverseApplyAlignment . applyAlignment" $
    prop "should be the identity" $ \alignment pos ->
      (reverseApplyAlignment alignment (applyAlignment alignment pos), pos) `shouldSatisfy` uncurry positionsEqual