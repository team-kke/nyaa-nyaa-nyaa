module Main where

import Control.Concurrent
import Timestamp (getTimestampRange)
import qualified Line as Line

delay :: Int
delay = 60 * 1000 * 1000 -- 1 min

main :: IO ()
main = do
  loopBody
  threadDelay delay
  main -- re run main until interrupted

loopBody :: IO ()
loopBody = do
  -- timestampRange <- getTimestampRange
  Line.send $ Line.Image "Nyaa Nyaa Nyaa" "http://i.imgur.com/rqWCyZ2.jpg"
