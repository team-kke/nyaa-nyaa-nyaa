module Project
  ( projectPath
  ) where

import System.Directory
import System.Environment (getArgs)
import System.Exit (exitSuccess)

projectPath :: IO FilePath
projectPath = do
  args <- getArgs
  case args of
    [dir] -> makeAbsolute dir
    _ -> do
      printUsage
      exitSuccess

printUsage :: IO ()
printUsage = do
  putStrLn "nyaa - the anime notifier\n"
  putStrLn "Usage: nyaa DIR\n"
  putStrLn "In DIR, there should be both 'config.yaml' and 'anime.yaml'."
  putStrLn "For the samples, please refer to the 'config' directory.\n"
