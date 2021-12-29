{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module GetTweet where

import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import Data.Maybe 
import Data.Foldable
import qualified Data.ByteString as B
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


entry :: String
entry = "https://api.twitter.com/1.1/"

getTweets :: String -> [(B.ByteString, Maybe B.ByteString)] -> OAuth -> Credential -> IO (Either String Searched)
getTweets url set_q oauth credential = do
    response <- do
        request <- parseRequest $ entry <> url
        
        let req = setQueryString set_q request 
        signedReq <- signOAuth oauth credential req
        manager   <- newManager tlsManagerSettings
        httpLbs signedReq manager
    return $ eitherDecode $ responseBody response

tweetText :: T.Text -> OAuth -> Credential -> IO ()
tweetText tw oauth credential = do
    req     <- parseRequest $ entry <> "statuses/update.json"
    manager <- newManager tlsManagerSettings
    let postReq = urlEncodedBody [("status", encodeUtf8 tw)] req
    signedReq <- signOAuth oauth credential postReq
    httpLbs signedReq manager
    return ()


-- ex set_q - [("include_entities", Just "true"), ("count", Just "100"), ("result_type", Just "recent")]
search ::  [(B.ByteString, Maybe B.ByteString)] -> T.Text -> OAuth -> Credential -> IO (Either String Searched)
search set_q text = getTweets "search/tweets.json" (("q", Just (encodeUtf8 text)) : set_q)

entitiedSearch :: T.Text -> OAuth -> Credential -> IO (Either String Searched)
entitiedSearch = search [("include_entities", Just "true"), ("count", Just "100"), ("result_type", Just "recent")]


extractMediaUrls :: Value -> V.Vector String
extractMediaUrls val = do
  let ms = val ^? key "extended_entities" . key "media" . _Array
      res = fmap T.unpack . (^? key "media_url_https" . _String) <$> fromMaybe V.empty ms  
    in V.catMaybes res

collectMediaUrls :: V.Vector Value -> V.Vector String
collectMediaUrls = foldMap extractMediaUrls