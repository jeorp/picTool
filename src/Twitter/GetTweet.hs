{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}


module Twitter.GetTweet where

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
import GHC.Generics
import Network.HTTP.Conduit
import Web.Authenticate.OAuth

import Control.Monad.Reader
import Twitter.Record
import Common

makeLenses ''Config

class HasToken c where
  entryl :: Lens' c String 
  oauthl :: Lens' c OAuth
  credentiall :: Lens' c Credential

instance HasToken Config where
  entryl = entry
  oauthl = oauth
  credentiall = credential

newtype Searched = Searched {statuses :: Value} deriving (Show, Generic)

instance FromJSON Searched
instance ToJSON Searched

entry' :: String
entry' = "https://api.twitter.com/1.1/"

getTweetsIO :: FromJSON a => String -> [(B.ByteString, Maybe B.ByteString)] -> OAuth -> Credential -> IO (Either String a)
getTweetsIO url set_q oauth credential = do
    response <- do
        request <- parseRequest $ entry' <> url
        
        let req = setQueryString set_q request 
        signedReq <- signOAuth oauth credential req
        manager   <- newManager tlsManagerSettings
        httpLbs signedReq manager
    return $ eitherDecode $ responseBody response

getTweets :: (MonadIO m, MonadReader r m, HasToken r, FromJSON a) => String -> [(B.ByteString, Maybe B.ByteString)] -> m (Either String a)
getTweets url set_q = do
  config <- ask
  let entry_ = config ^. entryl
      oauth_ = config ^. oauthl
      credential_ = config ^. credentiall
  response <- do
    liftIO $ do
      request <- parseRequest $ entry_ <> url
      let req = setQueryString set_q request 
      signedReq <- signOAuth oauth_ credential_ req
      manager   <- newManager tlsManagerSettings
      httpLbs signedReq manager
  return $ eitherDecode $ responseBody response

tweetTextIO :: T.Text -> OAuth -> Credential -> IO ()
tweetTextIO tw oauth credential = do
    req     <- parseRequest $ entry' <> "statuses/update.json"
    manager <- newManager tlsManagerSettings
    let postReq = urlEncodedBody [("status", encodeUtf8 tw)] req
    signedReq <- signOAuth oauth credential postReq
    httpLbs signedReq manager
    return ()


-- ex set_q - [("include_entities", Just "true"), ("count", Just "100"), ("result_type", Just "recent")]
searchIO ::  [(B.ByteString, Maybe B.ByteString)] -> T.Text -> OAuth -> Credential -> IO (Either String Searched)
searchIO set_q text = getTweetsIO "search/tweets.json" (("q", Just (encodeUtf8 text)) : set_q)

search :: (MonadIO m, MonadReader r m, HasToken r, FromJSON a) => 
               [(B.ByteString, Maybe B.ByteString)] -> T.Text -> m (Either String a)
search set_q text = getTweets "search/tweets.json" (("q", Just (encodeUtf8 text)) : set_q)

entitiedSearchIO :: T.Text -> OAuth -> Credential -> IO (Either String Searched)
entitiedSearchIO = searchIO [("include_entities", Just "true"), ("count", Just "100"), ("result_type", Just "recent")]

entitiedSearch :: (MonadIO m, MonadReader r m, HasToken r, FromJSON a) => 
               T.Text -> m (Either String a)
entitiedSearch = search [("include_entities", Just "true"), ("count", Just "100"), ("result_type", Just "recent")]

extractMediaUrls :: Value -> V.Vector String
extractMediaUrls val =
  let ms = val ^? key "extended_entities" . key "media" . _Array
      res = fmap T.unpack . (^? key "media_url_https" . _String) <$> fromMaybe V.empty ms  
    in V.catMaybes res

collectMediaUrls :: V.Vector Value -> V.Vector String
collectMediaUrls = foldMap extractMediaUrls

tweetUserInfo :: Value -> Maybe User
tweetUserInfo val = 
  let ms = val ^? key "user"
  in join $ decode . encode <$> ms

collectUserInfo :: V.Vector Value -> V.Vector User
collectUserInfo = V.mapMaybe tweetUserInfo
