module Config where

import Options.Applicative

data Config = Config
  { workingDirectory :: FilePath,
    lightFramesDirectory :: FilePath,
    darkFramesDirectory :: Maybe FilePath,
    biasFramesDirectory :: Maybe FilePath,
    flatFramesDirectory :: Maybe FilePath
  }
  deriving (Show)

configParser :: Parser Config
configParser =
  Config
    <$> workingDirParser
    <*> lightFramesDirectoryParser
    <*> darkFramesDirectoryParser
    <*> biasFramesDirectoryParser
    <*> flatFramesDirectoryParser

flatFramesDirectoryParser :: Parser (Maybe FilePath)
flatFramesDirectoryParser =
  optional $
    strOption
      ( long "flat-frames"
          <> short 'f'
          <> metavar "FLAT_FRAMES"
          <> help "Directory with flat frames for stacking."
      )

biasFramesDirectoryParser :: Parser (Maybe FilePath)
biasFramesDirectoryParser =
  optional $
    strOption
      ( long "bias-frames"
          <> short 'b'
          <> metavar "BIAS_FRAMES"
          <> help "Directory with bias frames for stacking."
      )

darkFramesDirectoryParser :: Parser (Maybe FilePath)
darkFramesDirectoryParser =
  optional $
    strOption
      ( long "dark-frames"
          <> short 'd'
          <> metavar "DARK_FRAMES"
          <> help "Directory with dark frames for stacking."
      )

lightFramesDirectoryParser :: Parser FilePath
lightFramesDirectoryParser =
  strOption
    ( long "light-frames"
        <> short 'l'
        <> metavar "LIGHT_FRAMES"
        <> help "Directory with light frames for stacking."
    )

workingDirParser :: Parser FilePath
workingDirParser =
  strOption
    ( long "working-dir"
        <> short 'w'
        <> metavar "WORKING_DIR"
        <> help "Working directory for temporary stacking files."
    )

parseConfig :: IO Config
parseConfig = execParser opts
  where
    opts =
      info
        (configParser <**> helper)
        fullDesc