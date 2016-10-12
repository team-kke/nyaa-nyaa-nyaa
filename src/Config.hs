{-# LANGUAGE OverloadedStrings #-}

module Config
  ( personalAccessToken
  ) where

import Data.Text
import Data.Yaml
import Project (projectPath)
import System.Exit (exitSuccess)
import System.FilePath.Posix

data Config = Config { getPersonalAccessToken :: Text }

instance FromJSON Config where
  parseJSON (Object v) = Config <$> v .: "personal-access-token"

config :: IO Config
config = do
  configPath <- (</> "config.yaml") <$> projectPath
  maybeDecodedConfig <- decodeFile configPath
  case maybeDecodedConfig of
    Just decodedConfig -> return decodedConfig
    otherwise -> do
      putStrLn "No valid config.yaml!"
      exitSuccess

personalAccessToken :: IO Text
personalAccessToken = getPersonalAccessToken <$> config
