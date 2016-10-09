module Arg
  ( ArgT
  , withArgs
  , animeYamlPath
  ) where

import Control.Monad.Trans.Reader
import System.Console.ArgParser (ParserSpec, withParseResult, reqFlag, parsedBy)

data Args = Args { getAnimeYamlPath :: String
                 }

type ArgT = ReaderT Args IO

argsParser :: ParserSpec Args
argsParser = Args
  `parsedBy` reqFlag "anime-yaml-path"

withArgs :: ArgT () -> IO ()
withArgs r = withParseResult argsParser (runReaderT r)

animeYamlPath :: ArgT String
animeYamlPath = getAnimeYamlPath <$> ask
