module Main where

import Lib

main :: IO ()
main = do
  s <- fetch "http://www.nyaa.se/?page=rss"
  putStr s
