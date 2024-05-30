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

-- main = parseConfig >>= runStacking

-- main = do
--   refStars <- map RefStar . take 400 . read <$> readFile "./resources/tmp/img_1_stars.txt"
--   targetStars <- map TargetStar . take 400 . read <$> readFile "./resources/tmp/img_2_stars.txt"
--   let Just r = computeLargeTriangleTransformation refStars targetStars
--   print $ length r
--   let uniques = M.toList $ M.fromListWith const r
--   print $ length uniques
--   let alignment = computeAlignment TranslationOnly $ map (bimap position position) uniques
--   print alignment
--   writeFile "./resources/tmp/img_2_alignment.txt" $ show alignment

main = testFoo

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
