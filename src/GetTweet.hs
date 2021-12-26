{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module GetTweet where

import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import Data.Maybe 
import Data.Foldable
import qualified Data.ByteString.Lazy.Char8 as S8
import qualified Data.Vector as V
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Data.Aeson.Encode.Pretty
import GHC.Generics
import Network.HTTP.Conduit
import qualified Network.HTTP.Simple as N
import Web.Authenticate.OAuth

newtype Tweet  = Tweet { text :: T.Text
                       } deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON Tweet

newtype Searched = Searched {statuses :: Value} deriving (Show, Generic)

instance FromJSON Searched
instance ToJSON Searched


myOAuth = newOAuth
    { oauthServerName     = "api.twitter.com"
    , oauthConsumerKey    = "lTkHxWpCxKXqtKzlT6iRV604y"    
    , oauthConsumerSecret = "v2ye0jycGBLcWaWxYctumxlO6vsGkqvynJEoSCSyQO35WOnwDY"  
    }

myCredential = newCredential
    "1208753502878482432-NdJumENTv165UqZLixL3r1njuO4Dxo"   
    "6qnp12Oknm6A6kObhfb1asTWzkjZ7gbFTilzBr2LsU4ZX" 

getPicTweet :: T.Text -> IO (Either String Searched)
getPicTweet q = do
    response <- do
        request <- parseRequest "https://api.twitter.com/1.1/search/tweets.json"
            -- ++ q -- ++ "&include_entities=true"
        let req = setQueryString [("q", Just (encodeUtf8 q)), ("include_entities", Just "true"), ("count", Just "100"), ("result_type", Just "recent")] request 
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

execute :: T.Text -> IO ()
execute q = do

  tl <- getPicTweet q
  case tl of
    Left err -> error err
    Right s -> do
      let vals = fromMaybe V.empty $ statuses s ^? _Array
          urls =  fold $ extractMediaUrls <$> vals
      print $ V.length vals
      mapM_ putStrLn urls

extractMediaUrls :: Value -> V.Vector String
extractMediaUrls val = do
  let ms = val ^? key "extended_entities" . key "media" . _Array
      res = fmap T.unpack . (^? key "media_url_https" . _String) <$> fromMaybe V.empty ms  
    in V.catMaybes res