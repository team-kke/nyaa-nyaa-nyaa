module Main where

import Anime
import Control.Concurrent
import Control.Exception (catch)
import Data.Time (getCurrentTime)
import Network.HTTP.Client (HttpException)
import Nyaa
import TimeRange
import qualified Line as Line

loop :: IO () -> IO ()
loop body = do
  body
  threadDelay interval
  loop body
  where
    interval = 10 * 60 * 1000 * 1000 -- 10 min

main :: IO ()
main = loop $ do
  range <- getTimeRange
  animeQueries <- getAnimeQueryList
  mapM_ (queryAndNotify range) animeQueries

queryAndNotify :: TimeRange -> AnimeQuery -> IO ()
queryAndNotify range query = do
  animes <- queryAnimeList range query `catch` printAndReturn []
  if length animes > 0
    then Line.send animes
    else return ()
  where
    printAndReturn :: a -> HttpException -> IO a
    printAndReturn x e = do
      now <- getCurrentTime
      putStrLn . concat $ [show now, ": ", show e]
      return x
