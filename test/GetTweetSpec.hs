{-# LANGUAGE OverloadedStrings #-}
module GetTweetSpec where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Foldable
import Data.Maybe 
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Web.Authenticate.OAuth
import GetTweet

myOAuth = newOAuth
    { oauthServerName     = "api.twitter.com"
    , oauthConsumerKey    = "lTkHxWpCxKXqtKzlT6iRV604y"    
    , oauthConsumerSecret = "v2ye0jycGBLcWaWxYctumxlO6vsGkqvynJEoSCSyQO35WOnwDY"  
    }

myCredential = newCredential
    "1208753502878482432-NdJumENTv165UqZLixL3r1njuO4Dxo"   
    "6qnp12Oknm6A6kObhfb1asTWzkjZ7gbFTilzBr2LsU4ZX" 

execute :: T.Text -> IO ()
execute q = do

  tl <- entitiedSearch q myOAuth myCredential
  case tl of
    Left err -> error err
    Right s -> do
      let vals = fromMaybe V.empty $ statuses s ^? _Array
          urls =  collectMediaUrls vals
      print $ V.length vals
      mapM_ putStrLn urls

main :: IO () 
main = hspec spec

spec :: Spec
spec = do
  describe "Test GetTweet" $ do
    it "test 1" $ do
      execute "from:ngnchiikawa" `shouldReturn` ()

