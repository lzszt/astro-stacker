{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Align
import AstroStacker
import Config
import Control.Concurrent.Async qualified as Async
import Data.Bifunctor
import Data.Map.Strict qualified as M
import Data.Maybe
import Locating
import Matching
import Types

main :: IO ()
-- main = generateTestTiffs
main = drawDSSStars
-- main = parseConfig >>= runStacking

-- main = do
--   refStars <- map RefStar . read <$> readFile "./resources/tmp/img_0_stars.txt"
--   targetStars <- map TargetStar . read <$> readFile "./resources/tmp/img_1_stars.txt"
--   alignment <- read <$> readFile "./resources/test_lights/align_1.txt"
--   let r = computeLargeTriangleTransformation refStars targetStars

--   let incorrects = filter (\(ref, tgt) -> applyAlignment alignment (position (toStar ref)) /= (position (toStar tgt))) r
--   mapM_ print incorrects
--   print $ length r
--   print $ length incorrects

-- (RefStar {unRef = Star {starPosition = Position {x = 107, y = 1262}, starRadius = 10.333630944789018}},TargetStar {unTarget = Star {starPosition = Position {x = 1747, y = 117}, starRadius = 9.09619407771256}})
-- (RefStar {unRef = Star {starPosition = Position {x = 416, y = 1514}, starRadius = 9.09619407771256}},TargetStar {unTarget = Star {starPosition = Position {x = 1438, y = 369}, starRadius = 9.31586399182265}})
-- (RefStar {unRef = Star {starPosition = Position {x = 1851, y = 1252}, starRadius = 10.605077554195745}},TargetStar {unTarget = Star {starPosition = Position {x = 1805, y = 1094}, starRadius = 9.824747468305834}})
-- (RefStar {unRef = Star {starPosition = Position {x = 2320, y = 2205}, starRadius = 9.022970773009195}},TargetStar {unTarget = Star {starPosition = Position {x = 2274, y = 141}, starRadius = 8.97119407771256}})
-- (RefStar {unRef = Star {starPosition = Position {x = 11, y = 252}, starRadius = 9.147970773009195}},TargetStar {unTarget = Star {starPosition = Position {x = 2566, y = 2394}, starRadius = 8.97119407771256}})

-- main = testFoo
-- main = test

-- targetStarss <- map (fmap (map TargetStar . read)) <$> mapM (\name -> (name,) <$> readFile ("./resources/tmp/" <> name <> ".txt")) ["img_2_stars"]
-- _ <-
--   catMaybes
--     <$> Async.mapConcurrently
--       ( \(name, targetStars) ->
--           let res = computeLargeTriangleTransformation refStars targetStars
--            in case res of
--                 Nothing -> pure Nothing
--                 Just r -> do
--                   print $ fmap length r
--                   let uniques = M.toList $ M.fromListWith const r
--                   print $ length uniques
--                   let alignment = computeAlignment TranslationOnly $ map (bimap position position) uniques
--                   print alignment
--                   writeFile ("./resources/tmp/" <> name <> "_alignment.txt") $ show alignment
--                   pure $ Just (name, alignment)
--       )
--       targetStarss
-- undefined
