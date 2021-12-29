{-# LANGUAGE OverloadedStrings #-}


module GetToken (extract) where
import Common

import Prelude hiding (readFile)
import Data.Text 
import Data.Text.Encoding
import qualified Data.ByteString as B
import Data.Char
import Data.Text.IO
import Data.Attoparsec.Text
import Web.Authenticate.OAuth

extractText :: Parser Text 
extractText = takeTill isEndOfLine 

channellabel :: Parser Text
channellabel = "entry:" *> extractText

discordBotTokenlabel :: Parser Text
discordBotTokenlabel = "discord_bot_token:" *> extractText

gmoApiKeyLable :: Parser Text
gmoApiKeyLable = "gmo_api_key:" *> extractText

gmoApiSecretLable :: Parser Text
gmoApiSecretLable = "gmo_api_secret:" *> extractText


configParser :: Parser Config
configParser = do 
  (a, b) <- (,) <$>
      channellabel <* endOfLine <*>
      discordBotTokenlabel <* endOfLine
  (c, d) <- (,) <$>        
      gmoApiKeyLable <* endOfLine <*>         
      gmoApiSecretLable
  let oa = newOAuth 
        {
          oauthServerName = "api.twitter.com",
          oauthConsumerKey = encodeUtf8 a,
          oauthConsumerSecret = encodeUtf8 b
        }
      cre = newCredential (encodeUtf8 c) (encodeUtf8 d)
    in pure $ Config oa cre


extract :: IO (Either String Config)
extract = parseOnly configParser <$> readFile configFile