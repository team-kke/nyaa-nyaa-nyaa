{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( fetch
    ) where

import Text.RSS.Types
import Text.RSS.Conduit.Parse
import Network.Wreq
import Control.Lens ((^.))
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (toStrict)
import Data.UnixTime (UnixTime, parseUnixTime, mailDateFormat)
import qualified Data.ByteString.Char8 as DBC
import qualified Data.List.Split as DLS

-- parsePubDate: function to parse pubDate field in RSS
--
--   parsePubDate "Mon, 10 Oct 2016 07:23:11 +0000"
--
-- The code above will return UnixTime for the pubDate.
parsePubDate :: String -> UnixTime
parsePubDate = parseUnixTime mailDateFormat . pack

-- The datetime query range will be in the type of (UnixTime, UnixTime).
-- The code below is just an example. please feel free to modify it.
queryAnimeList :: (UnixTime, UnixTime) -> String -> [anime]
queryAnimeList (since, until) animeName = undefined

-- Please remove the verbose comments above after understanding them.

fetch :: String -> IO String
fetch s = do
  r <- get s
  return $ DBC.unpack $ toStrict $ r ^. responseBody

splitByNewLine :: String -> [String]
splitByNewLine s = DLS.splitOn "\n" s
