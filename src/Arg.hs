module Arg
  ( ArgReaderT
  , withArgs
  , animeYamlPath
  ) where

import Control.Monad.Trans.Reader
import System.Console.ArgParser (ParserSpec, withParseResult, reqFlag, parsedBy)

data Args = Args { getAnimeYamlPath :: String
                 }

type ArgReaderT = ReaderT Args IO

argsParser :: ParserSpec Args
argsParser = Args
  `parsedBy` reqFlag "anime-yaml-path"

withArgs :: ArgReaderT () -> IO ()
withArgs r = withParseResult argsParser (runReaderT r)

animeYamlPath :: ArgReaderT String
animeYamlPath = getAnimeYamlPath <$> ask
