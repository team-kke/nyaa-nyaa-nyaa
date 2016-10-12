{-# LANGUAGE OverloadedStrings #-}

module Nyaa
  ( fetch
  , fetchAndParseRSS
  , findItems
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
import Data.List (isInfixOf)

import URI.ByteString (URIRef, Absolute)

import qualified Data.Text as DT

import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as DCL
import Data.Conduit.Parser

import Text.XML.Stream.Parse as XML hiding (choose)

data Anime = Anime { title :: DT.Text
                   , torrentLink :: Maybe (URIRef Absolute)
                   , detailLink :: Maybe (URIRef Absolute)
                   } deriving (Show)

-- parsePubDate: function to parse pubDate field in RSS
--
--   parsePubDate "Mon, 10 Oct 2016 07:23:11 +0000"
--
-- The code above will return UnixTime for the pubDate.
parsePubDate :: String -> UnixTime
parsePubDate = parseUnixTime mailDateFormat . pack

-- The datetime query range will be in the type of (UnixTime, UnixTime).
-- The code below is just an example. please feel free to modify it.
--  queryAnimeList :: (UnixTime, UnixTime) -> String -> IO [Anime]
--  queryAnimeList (since, until) animeName = do
--    rss <- fetchAndParseRSS "http://nyaa.se://www.nyaa.se/?page=rss"
--    return $ map toAnime (findItems rss animeName)

--  toAnime :: RssItem -> Anime
--  toAnime r = Anime { title=(itemTitle r), torrentLink=(itemLink r => (\r -> )), detailLink=(itemGuid r) }

toURIRef:: RssURI -> URIRef Absolute
toURIRef (RssURI a) = a

-- Please remove the verbose comments above after understanding them.



fetch :: String -> IO String
fetch s = do
  r <- get s
  return $ DBC.unpack $ toStrict $ r ^. responseBody

splitByNewLine :: String -> [String]
splitByNewLine s = DLS.splitOn "\n" s

fetchAndParseRSS :: String -> IO RssDocument
fetchAndParseRSS url = do
  s <- fetch url
  let input = map DT.pack (splitByNewLine s)
  r <- DCL.sourceList input =$= XML.parseText' def $$ runConduitParser rssDocument
  return r

findItems :: RssDocument -> String -> [RssItem]
findItems document query = filter (titleContain query) (channelItems document)

titleContain :: String -> RssItem -> Bool
titleContain query item = isInfixOf query (DT.unpack $ itemTitle item)
