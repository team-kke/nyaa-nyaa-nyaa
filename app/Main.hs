module Main where

import Anime
import Control.Concurrent
import Nyaa
import TimeRange
import qualified Line as Line

interval :: Int
interval = 10 * 60 * 1000 * 1000 -- 10 min

main :: IO ()
main = do
  loop interval body

loop :: Int -> IO () -> IO ()
loop interval body = do
  body
  threadDelay interval
  loop interval body

body :: IO ()
body = do
  range <- getTimeRange
  animeQueries <- getAnimeQueryList
  mapM_ (queryAndNotify range) animeQueries

queryAndNotify :: TimeRange -> AnimeQuery -> IO ()
queryAndNotify range query = do
  animes <- queryAnimeList range query
  if length animes > 0
    then Line.send animes
    else return ()
