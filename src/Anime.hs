module Anime
  ( AnimeQuery
  , getAnimeQueryList
  ) where

import Project (projectPath)
import System.Exit (exitSuccess)
import System.FilePath.Posix
import Data.Text (unpack)
import Data.Yaml (decodeFile)

type AnimeQuery = String

getAnimeQueryList :: IO [AnimeQuery]
getAnimeQueryList = do
  animePath <- (</> "anime.yaml") <$> projectPath
  maybeResult <- decodeFile animePath
  case maybeResult of
    Just result -> return $ map unpack result
    otherwise -> do
      putStrLn "No valid anime.yaml!"
      exitSuccess
