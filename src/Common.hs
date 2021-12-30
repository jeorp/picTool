{-# LANGUAGE OverloadedStrings #-}

module Common where

import Data.Text
import qualified Data.ByteString as B
import Web.Authenticate.OAuth

configFile :: String
configFile = "config.env"

data Config = Config 
  {
    _entry :: String,
    _oauth :: OAuth,
    _credential :: Credential
  } deriving (Eq, Show)

defaultConfig = Config 
    {
      _entry = "",
      _oauth = newOAuth 
        {
          oauthServerName = "",
          oauthConsumerKey = "",
          oauthConsumerSecret = ""
        },
      _credential = newCredential "" ""
    }

