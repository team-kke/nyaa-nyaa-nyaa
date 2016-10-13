{-# LANGUAGE OverloadedStrings #-}

module Line
  ( Message (..)
  , send
  , Messageable (..)
  ) where

import Prelude hiding (append)
import Config (personalAccessToken)
import Control.Lens
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Network.Wreq
import System.Environment
import qualified Data.ByteString as B

type PersonalAccessToken = Text

data Message = Text { text :: Text }
             | Image { text :: Text, imageURL :: Text }

class Messageable a where
  toText :: a -> Text
  toText = undefined

  toMessage :: a -> Message
  toMessage = Text . toText

instance Messageable Message where
  toMessage = id

body :: Message -> [FormParam]
body (Text text) = [ "message" := "\n" `append` text ]
body (Image text imageURL) = [ "message" := "\n" `append` text
                             , "imageFullsize" := imageURL
                             , "imageThumbnail" := imageURL
                             ]

option :: PersonalAccessToken -> Options
option token = defaults & header "Authorization" .~ ["Bearer " `B.append` (encodeUtf8 token)]

send :: Messageable a => a -> IO ()
send messageable = do
  let message = toMessage messageable
  token <- personalAccessToken
  postWith (option token) "https://notify-api.line.me/api/notify" (body message)
  return ()
