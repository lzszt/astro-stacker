module Main where

-- import StarMatching

import AstroStacker
import Config
import Data.Maybe
import Effects.Logging qualified as Log
import Locating
import System.Environment (getArgs)

main :: IO ()
main = do
  config <- parseConfig
  print config

-- (dark : bias : lights : workingDir : remArgs) <- getArgs
-- darks <- locateFiles dark
-- biass <- locateFiles bias
-- lightss <- locateFiles lights

-- let severity = fromMaybe Log.Info (Log.parseSeverity =<< headMay remArgs)

-- run severity darks biass lightss workingDir

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x