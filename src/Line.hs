{-# LANGUAGE OverloadedStrings #-}

module Line
  ( Message (..)
  , send
  , toMessage
  ) where

import Anime
import Config (personalAccessToken)
import Control.Lens
import Data.ByteString
import Data.Text hiding (append)
import Data.Text.Encoding (encodeUtf8)
import Network.Wreq
import System.Environment

type PersonalAccessToken = Text

data Message = Text { text :: Text }
             | Image { text :: Text, imageURL :: Text }

body :: Message -> [FormParam]
body (Text text) = [ "message" := text ]
body (Image text imageURL) = [ "message" := text
                             , "imageFullsize" := imageURL
                             , "imageThumbnail" := imageURL
                             ]

option :: PersonalAccessToken -> Options
option token = defaults & header "Authorization" .~ ["Bearer " `append` (encodeUtf8 token)]

send :: Message -> IO ()
send message = do
  token <- personalAccessToken
  postWith (option token) "https://notify-api.line.me/api/notify" (body message)
  return ()

toMessage :: Anime -> Message
toMessage a = Text $ "New!\n" ++ show a
