{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( fetch
    ) where

import Text.RSS.Types
import Text.RSS.Conduit.Parse
import Network.Wreq
import Control.Lens ((^.))
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Char8 as DBC
import qualified Data.List.Split as DLS

fetch :: String -> IO String
fetch s = do
  r <- get s
  return $ DBC.unpack $ toStrict $ r ^. responseBody

splitByNewLine :: String -> [String]
splitByNewLine s = DLS.splitOn "\n" s
