{-# LANGUAGE GADTs #-}

module Anime
  ( AnimeQuery
  , getAnimeQueryList
  , Anime
  , toAnime
  ) where

import Project (projectPath)
import System.Exit (exitSuccess)
import System.FilePath.Posix
import Data.Text (Text)
import Data.Yaml (decodeFile)
import URI.ByteString (URIRef(..), Absolute, RelativeRef, serializeURIRef')
import Text.RSS.Types (RssItem(..), RssURI(..), RssGuid(..))
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BS

data Anime = Anime { title :: Text
                   , torrentLink :: Maybe (URIRef Absolute)
                   , detailLink :: Maybe (URIRef Absolute)
                   }

instance Show Anime where
  show (Anime title torrent detail) = "Title: " ++ unpack title ++ "\n" ++ "Torrent: " ++ stringifyURIRef torrent

stringifyURIRef :: Maybe (URIRef Absolute) -> String
stringifyURIRef m = fromMaybe "none" (fmap (BS.unpack . serializeURIRef') m)

toAnime :: RssItem -> Anime
toAnime r = Anime { title=(itemTitle r), torrentLink=((itemLink r) >>= toURIRef), detailLink=((itemGuid r) >>= toURIRef) }

class URIConvertible a where
  toURIRef :: a -> Maybe (URIRef Absolute)

instance URIConvertible RssURI where
  toURIRef (RssURI (RelativeRef _ _ _ _)) = Nothing
  toURIRef (RssURI (URI a b c d e)) = Just (URI a b c d e)

instance URIConvertible RssGuid where
  toURIRef (GuidUri r) = toURIRef r
  toURIRef _ = Nothing

type AnimeQuery = Text

getAnimeQueryList :: IO [AnimeQuery]
getAnimeQueryList = do
  animePath <- (</> "anime.yaml") <$> projectPath
  maybeResult <- decodeFile animePath
  case maybeResult of
    Just result -> return result
    otherwise -> do
      putStrLn "No valid anime.yaml!"
      exitSuccess
