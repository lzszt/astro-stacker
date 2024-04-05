module Main where

-- import StarMatching

import AstroStacker
import Config

main :: IO ()
main = parseConfig >>= runStacking