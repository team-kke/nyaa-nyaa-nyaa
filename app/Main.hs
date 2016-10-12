module Main where

import Control.Concurrent
import Timestamp (getTimestampRange)
import qualified Line as Line
import Nyaa

interval :: Int
interval = 60 * 1000 * 1000 -- 1 min

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
  r <- fetchAndParseRSS "https://www.nyaa.se/?page=rss"
  let items = findItems r "Eupho"
  print $ show $ items
  -- timestampRange <- getTimestampRange
--    print channelTitle r
--    Line.send $ Line.Image "Nyaa Nyaa Nyaa" "http://i.imgur.com/rqWCyZ2.jpg"
