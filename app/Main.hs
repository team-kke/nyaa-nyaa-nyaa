module Main where

import Arg
import Control.Monad.IO.Class
import Control.Concurrent

delay :: Int
delay = 60 * 1000 * 1000 -- 1 min

main :: IO ()
main = withArgs $ do
  loopBody
  liftIO $ do
    threadDelay delay
    main -- re run main until interrupted

loopBody :: ArgReaderT ()
loopBody = do
  yamlPath <- animeYamlPath
  liftIO $ putStrLn yamlPath -- FIXME
