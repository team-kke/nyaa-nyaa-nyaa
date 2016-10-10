module Anime where
  -- ( Anime (...)
  -- , getAnimes
  -- ) where

-- import Data.Yaml

data Anime = Anime { name :: String } deriving Show

getAnimes :: IO [Anime]
getAnimes = undefined
