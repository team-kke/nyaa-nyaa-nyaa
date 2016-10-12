module Main where

import Anime
import Control.Concurrent
import Data.UnixTime
import Nyaa
import Timestamp
import qualified Line as Line

interval :: Int
interval = 5 * 60 * 1000 * 1000 -- 5 min

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
  range <- getTimestampRange
  animeQueries <- getAnimeQueryList
  mapM_ (queryAndNotify range) animeQueries

queryAndNotify :: (UnixTime, UnixTime) -> AnimeQuery -> IO ()
queryAndNotify range query =
  -- FIXME: uncomment following lines and remove return ()
  -- animes <- queryAnimeList range query
  -- mapM_ notifyAnime animes
  return ()

-- FIXME: Rename anime as Anime, and replace a2m with whatever you implemented
notifyAnime :: anime -> IO ()
notifyAnime = Line.send . a2m

-- FIXME: Remove the following function
a2m :: anime -> Line.Message
a2m = undefined
