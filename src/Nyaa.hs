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
import Data.Text (Text, splitOn, isInfixOf)
import Data.Text.Encoding (decodeUtf8)
import Network.Wreq
import Text.RSS.Conduit.Parse
import Text.RSS.Types
import Text.XML.Stream.Parse as XML hiding (choose)
import TimeRange
import qualified Data.Conduit.List as CL

(.&&.) = liftM2 (&&)

queryAnimeList :: TimeRange -> AnimeQuery -> IO [Anime]
queryAnimeList range query = do
  rss <- fetchAndParseRSS query
  return $ map toAnime $ filter (isRaw .&&. inRange range) (channelItems rss)

fetch :: AnimeQuery -> IO Text
fetch query = do
  r <- getWith options "http://nyaa.se/?page=rss"
  return . decodeUtf8 . toStrict $ r ^. responseBody
  where
    options = defaults
      & param "page" .~ ["rss"]
      & param "cats" .~ ["1_11"]
      & param "term" .~ [query]

splitByNewLine :: Text -> [Text]
splitByNewLine s = splitOn "\n" s

fetchAndParseRSS :: AnimeQuery -> IO RssDocument
fetchAndParseRSS query = do
  t <- fetch query
  let input = splitByNewLine t
  CL.sourceList input =$= XML.parseText' def $$ runConduitParser rssDocument

isRaw :: RssItem -> Bool
isRaw item = "Raws" `isInfixOf` (itemTitle item)

inRange :: TimeRange -> RssItem -> Bool
inRange (since, til) item =
  case itemPubDate item of
    Just x -> since < x && x < til
    _ -> False
