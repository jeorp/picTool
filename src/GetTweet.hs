{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GetTweet where

import Data.Text
import Data.Text.Encoding
import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Web.Authenticate.OAuth

newtype Tweet  = Tweet { text :: Text
                       } deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON Tweet

myName = "jhonda_bot" -- botのTwitterアカウント名

myOAuth = newOAuth
    { oauthServerName     = "api.twitter.com"
    , oauthConsumerKey    = "your consumer key"     -- https://apps.twitter.com/ で取得したやつ
    , oauthConsumerSecret = "your consumer secret"  -- https://apps.twitter.com/ で取得したやつ
    }

myCredential = newCredential
    "your access token"        -- https://apps.twitter.com/ で取得したやつ
    "your access token secret" -- https://apps.twitter.com/ で取得したやつ

getHomeTL :: IO (Either String [Tweet])
getHomeTL = do
    response <- do
        req <-
            parseRequest
            $ "https://api.twitter.com/1.1/statuses/home_timeline.json?screen_name="
            ++ myName
        signedReq <- signOAuth myOAuth myCredential req
        manager   <- newManager tlsManagerSettings
        httpLbs signedReq manager
    return $ eitherDecode $ responseBody response

tweet :: Text -> IO ()
tweet tw = do
    req     <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
    manager <- newManager tlsManagerSettings
    let postReq = urlEncodedBody [("status", encodeUtf8 tw)] req
    signedReq <- signOAuth myOAuth myCredential postReq
    httpLbs signedReq manager
    return ()