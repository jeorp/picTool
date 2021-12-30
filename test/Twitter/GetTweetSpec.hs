{-# LANGUAGE OverloadedStrings #-}
module Twitter.GetTweetSpec where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B 
import qualified Data.Vector as V
import Data.Foldable
import Data.Maybe 
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Lens
import Control.Lens
import Web.Authenticate.OAuth
import Twitter.GetTweet

myOAuth = newOAuth
    { oauthServerName     = "api.twitter.com"
    , oauthConsumerKey    = "XXXXXXXXXXXXXXXXXXXX"    
    , oauthConsumerSecret = "YYYYYYYYYYYYYYYYYYYYYYY"  
    }

myCredential = newCredential
    "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"   
    "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" 

executeSearchIO :: T.Text -> IO ()
executeSearchIO q = do

  tl <- entitiedSearchIO q myOAuth myCredential
  case tl of
    Left err -> error err
    Right s -> do
      let vals = fromMaybe V.empty $ statuses s ^? _Array
          urls =  collectMediaUrls vals
      --B.putStrLn $ encodePretty $ V.head vals
      print $ tweetUserInfo $ V.head vals 
      mapM_ putStrLn urls

extractTweetUserInfo :: T.Text -> IO ()
extractTweetUserInfo q = do

  tl <- entitiedSearchIO q myOAuth myCredential
  case tl of
    Left err -> error err
    Right s -> do
      let vals = fromMaybe V.empty $ statuses s ^? _Array
          users =  collectUserInfo vals
      mapM_ print users



main :: IO () 
main = hspec spec

spec :: Spec
spec = do
  describe "Test GetTweet" $ do
    it "test 1" $ do
      executeSearchIO "from:ngnchiikawa" `shouldReturn` ()

    it "test 2" $ do
      extractTweetUserInfo "from:ngnchiikawa" `shouldReturn` ()