{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GetTweet where

import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T

import qualified Data.ByteString.Lazy.Char8 as S8

import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Generics
import Network.HTTP.Conduit
import qualified Network.HTTP.Simple as N
import Web.Authenticate.OAuth

newtype Tweet  = Tweet { text :: T.Text
                       } deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON Tweet

myName = "jhonda_bot" -- botのTwitterアカウント名

myOAuth = newOAuth
    { oauthServerName     = "api.twitter.com"
    , oauthConsumerKey    = "lTkHxWpCxKXqtKzlT6iRV604y"    -- https://apps.twitter.com/ で取得したやつ
    , oauthConsumerSecret = "v2ye0jycGBLcWaWxYctumxlO6vsGkqvynJEoSCSyQO35WOnwDY"  -- https://apps.twitter.com/ で取得したやつ
    }

myCredential = newCredential
    "1208753502878482432-NdJumENTv165UqZLixL3r1njuO4Dxo"        -- https://apps.twitter.com/ で取得したやつ
    "6qnp12Oknm6A6kObhfb1asTWzkjZ7gbFTilzBr2LsU4ZX" -- https://apps.twitter.com/ で取得したやつ

getHomeTL :: IO (Either String [Value])
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

getPicTweet :: T.Text -> IO (Either String Value)
getPicTweet q = do
    response <- do
        request <- parseRequest "https://api.twitter.com/1.1/search/tweets.json"
            -- ++ q -- ++ "&include_entities=true"
        let req = setQueryString [("q", Just (encodeUtf8 q)), ("include_entities", Just "true")] request 
        signedReq <- signOAuth myOAuth myCredential req
        manager   <- newManager tlsManagerSettings
        httpLbs signedReq manager
    --print response
    return $ eitherDecode $ responseBody response

tweet :: T.Text -> IO ()
tweet tw = do
    req     <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
    manager <- newManager tlsManagerSettings
    let postReq = urlEncodedBody [("status", encodeUtf8 tw)] req
    signedReq <- signOAuth myOAuth myCredential postReq
    httpLbs signedReq manager
    return ()

execute :: IO ()
execute = do
  -- とりあえずホームタイムラインの最新10件を表示する
  homeTL <- getPicTweet "from:naco_miyasaka filter:images"
  case homeTL of
    Left err -> error err
    Right tl -> S8.putStrLn $ encodePretty $ tl
