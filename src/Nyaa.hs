{-# LANGUAGE OverloadedStrings #-}

module Nyaa
  ( fetch
  , fetchAndParseRSS
  , queryAnimeList
  ) where

import Anime
import Control.Lens ((^.), (&), (.~))
import Control.Monad (liftM2)
import Data.ByteString.Lazy (toStrict)
import Data.Conduit
import Data.Conduit.Parser
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.UnixTime
import Network.Wreq
import Text.RSS.Conduit.Parse
import Text.RSS.Types
import Text.XML.Stream.Parse as XML hiding (choose)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.List as DCL
import qualified Data.Text as T

(.&&.) = liftM2 (&&)

queryAnimeList :: (UnixTime, UnixTime) -> AnimeQuery -> IO [Anime]
queryAnimeList range query = do
  rss <- fetchAndParseRSS query
  return $ map toAnime $ filter (isRaw .&&. inRange range) (channelItems rss)

fetch :: AnimeQuery -> IO String
fetch query = do
  r <- getWith options "http://nyaa.se/?page=rss"
  return . BS.unpack .toStrict $ r ^. responseBody
  where
    options = defaults
      & param "page" .~ ["rss"]
      & param "cats" .~ ["1_11"]
      & param "cats" .~ [T.pack query]

splitByNewLine :: String -> [String]
splitByNewLine s = splitOn "\n" s

fetchAndParseRSS :: AnimeQuery -> IO RssDocument
fetchAndParseRSS query = do
  s <- fetch query
  let input = map T.pack (splitByNewLine s)
  r <- DCL.sourceList input =$= XML.parseText' def $$ runConduitParser rssDocument
  return r

isRaw :: RssItem -> Bool
isRaw item = isInfixOf "Raws" (T.unpack $ itemTitle item)

inRange :: (UnixTime, UnixTime) -> RssItem -> Bool
inRange (since, til) item =
  case itemPubDate item of
    Just utc -> let t = timestamp utc in since < t && t < til
    otherwise -> False
  where
    rfc2616 = "%a, %d %b %Y %H:%M:%S GMT"
    timestamp = parseUnixTimeGMT webDateFormat . BS.pack . formatTime defaultTimeLocale rfc2616
