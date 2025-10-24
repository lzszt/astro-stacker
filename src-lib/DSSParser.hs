{-# LANGUAGE OverloadedStrings #-}
module DSSParser where
  
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import qualified Data.Text.IO 



-- Define a parser type
type Parser = Parsec Void Text

-- Data types for global properties and star info
data GlobalProps = GlobalProps
  { overallQuality :: Double
  , redXShift      :: Double
  , redYShift      :: Double
  , blueXShift     :: Double
  , blueYShift     :: Double
  , skyBackground  :: Double
  , nrStars        :: Int
  } deriving Show

data StarInfo = StarInfo
  { starNumber    :: Int
  , intensity     :: Double
  , quality       :: Double
  , meanRadius    :: Double
  , rect          :: (Int, Int, Int, Int)
  , center        :: (Double, Double)
  , axises        :: (Double, Double, Double, Double, Double)
  } deriving Show

-- Main data structure
data StarData = StarData GlobalProps [StarInfo] deriving Show

-- Parser for a key-value pair of Double values
parseDoublePair :: Parser (String, Double)
parseDoublePair = do
  key <- some (alphaNumChar <|> char '#')
  _ <- spaceChar *> char '=' *> spaceChar
  value <- L.signed space L.float
  return (key, value)

-- Parser for Rect, Center, Axises
parseIntTuple :: Parser (Int, Int, Int, Int)
parseIntTuple = do
  _ <- string "Rect = "
  x1 <- L.decimal
  _ <- string ", "
  y1 <- L.decimal
  _ <- string ", "
  x2 <- L.decimal
  _ <- string ", "
  y2 <- L.decimal
  crlf
  return (x1, y1, x2, y2)

parseDoubleTuple :: Parser (Double, Double)
parseDoubleTuple = do
  _ <- string "Center = "
  x <- L.float
  _ <- string ", "
  y <- L.float
  crlf
  return (x, y)

parseAxises :: Parser (Double, Double, Double, Double, Double)
parseAxises = do
  _ <- string "Axises = "
  a <- L.float
  _ <- string ", "
  b <- L.float
  _ <- string ", "
  c <- L.float
  _ <- string ", "
  d <- L.float
  _ <- string ", "
  e <- L.float
  return (a, b, c, d, e)

-- Parser for Global Properties
parseGlobalProps :: Parser GlobalProps
parseGlobalProps = do
  overall <- parseKeyValueFloat "OverallQuality"
  redX <- parseKeyValueFloat "RedXShift"
  redY <- parseKeyValueFloat "RedYShift"
  blueX <- parseKeyValueFloat "BlueXShift"
  blueY <- parseKeyValueFloat "BlueYShift"
  sky <- parseKeyValueFloat "SkyBackground"
  nr <- parseKeyValueInt ("NrStars")
  return $ GlobalProps overall redX redY blueX blueY sky  nr
  
  
parseKeyValueFloat key = do
  _ <- string (key <> " = ")
  f <- L.float
  _ <- crlf
  pure f

parseKeyValueInt key = do
  _ <- string (key <> " = ")
  f <- L.decimal
  _ <- crlf
  pure f
parseKeyValueSigned key = do
  _ <- string (key <> " = ")
  f <- L.signed space L.float
  _ <- crlf
  pure f

-- Parser for one star's info
parseStarInfo :: Parser StarInfo
parseStarInfo = do
  starNo <- parseKeyValueInt "Star#"
  intensity <- parseKeyValueFloat "Intensity"
  quality <- parseKeyValueSigned "Quality"
  radius <- parseKeyValueFloat "MeanRadius"
  rect <- parseIntTuple
  center <- parseDoubleTuple
  axises <- parseAxises
  crlf
  return $ StarInfo starNo intensity quality radius rect center axises
  
-- Main parser
parseStarData :: Parser StarData
parseStarData = do
  globalProps <- parseGlobalProps
  stars <- many parseStarInfo
  return $ StarData globalProps stars

-- Helper function to run parser
parseData :: Text -> Either (ParseErrorBundle Text Void) StarData
parseData = runParser parseStarData ""

-- Example usage
main :: IO ()
main = do
  input <- Data.Text.IO.readFile "./resources/M82/2018-06-20/lights/DSC00580.Info.txt"
  case parseData input of
    Left err -> putStrLn $ errorBundlePretty err
    Right result -> print result

loadDSSStarData :: FilePath -> IO StarData
loadDSSStarData path = do
  input <- Data.Text.IO.readFile path
  case parseData input of
    Left err -> error $ errorBundlePretty err
    Right result -> pure result
