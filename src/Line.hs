{-# LANGUAGE OverloadedStrings #-}

module Line
  ( Message (..)
  , send
  ) where

import Config (personalAccessToken)
import Control.Lens
import Data.ByteString hiding (pack)
import Data.ByteString.Char8
import Network.Wreq
import System.Environment

type PersonalAccessToken = String

data Message = Text { text :: String }
             | Image { text :: String, imageURL :: String }

body :: Message -> [FormParam]
body (Text text) = [ "message" := text ]
body (Image text imageURL) = [ "message" := text
                             , "imageFullsize" := imageURL
                             , "imageThumbnail" := imageURL
                             ]

option :: PersonalAccessToken -> Options
option token = defaults & header "Authorization" .~ [pack $ "Bearer " ++ token]

send :: Message -> IO ()
send message = do
  token <- personalAccessToken
  postWith (option token) "https://notify-api.line.me/api/notify" (body message)
  return ()
