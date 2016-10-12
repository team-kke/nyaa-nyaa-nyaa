{-# LANGUAGE OverloadedStrings #-}

module Nyaa
  ( fetch
  , fetchAndParseRSS
  , findItems
  , queryAnimeList
  ) where

import Text.RSS.Types
import Text.RSS.Conduit.Parse
import Network.Wreq
import Control.Lens ((^.))
import Data.ByteString.Lazy (toStrict)
import Data.UnixTime (UnixTime, parseUnixTime, mailDateFormat)
import Data.List.Split (splitOn)
import Data.List (isInfixOf)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import Data.Conduit
import qualified Data.Conduit.List as DCL
import Data.Conduit.Parser

import Text.XML.Stream.Parse as XML hiding (choose)

import Anime

-- parsePubDate: function to parse pubDate field in RSS
--
--   parsePubDate "Mon, 10 Oct 2016 07:23:11 +0000"
--
-- The code above will return UnixTime for the pubDate.
parsePubDate :: String -> UnixTime
parsePubDate = parseUnixTime mailDateFormat . BS.pack

-- The datetime query range will be in the type of (UnixTime, UnixTime).
-- The code below is just an example. please feel free to modify it.
queryAnimeList :: (UnixTime, UnixTime) -> String -> IO [Anime]
queryAnimeList (since, until) animeName = do
  rss <- fetchAndParseRSS "http://nyaa.se/?page=rss"
  return $ map toAnime (findItems rss animeName)

-- Please remove the verbose comments above after understanding them.

fetch :: String -> IO String
fetch s = do
  r <- get s
  return $ BS.unpack $ toStrict $ r ^. responseBody

splitByNewLine :: String -> [String]
splitByNewLine s = splitOn "\n" s

fetchAndParseRSS :: String -> IO RssDocument
fetchAndParseRSS url = do
  s <- fetch url
  let input = map T.pack (splitByNewLine s)
  r <- DCL.sourceList input =$= XML.parseText' def $$ runConduitParser rssDocument
  return r

findItems :: RssDocument -> String -> [RssItem]
findItems document query = filter (titleContain query) (channelItems document)

titleContain :: String -> RssItem -> Bool
titleContain query item = isInfixOf query (T.unpack $ itemTitle item)
