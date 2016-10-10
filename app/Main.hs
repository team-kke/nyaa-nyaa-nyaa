module Main where

import Arg
import Control.Concurrent
import Timestamp (getTimestampRange)

delay :: Int
delay = 60 * 1000 * 1000 -- 1 min

main :: IO ()
main = do
  loopBody
  threadDelay delay
  main -- re run main until interrupted

loopBody :: IO ()
loopBody = do
  timestampRange <- getTimestampRange
  print timestampRange
