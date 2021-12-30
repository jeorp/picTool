{-# LANGUAGE OverloadedStrings #-}


module GetToken (extract) where
import Common

import Prelude hiding (readFile)
import Data.Text 
import Data.Text.Encoding
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Text.IO
import Data.Attoparsec.Text
import Web.Authenticate.OAuth

extractText :: Parser Text 
extractText = takeTill isEndOfLine 

{- entry:WWWWWWW
oauthServerName:TGTFTFTFTFTFT
authConsumerKey:QEEEEEE
oauthConsumerSecret:QQQQQQQ
token:SSSSSSSSSS
tokenSecret:AAAAAAAAAA
-}
entryLabel :: Parser Text
entryLabel = "entry:" *> extractText

oauthServerNameLabel :: Parser Text
oauthServerNameLabel = "oauthServerName:" *> extractText

authConsumerKeyLabel :: Parser Text
authConsumerKeyLabel = "authConsumerKey:" *> extractText

oauthConsumerSecretLabel :: Parser Text
oauthConsumerSecretLabel = "oauthConsumerSecret:" *> extractText

tokenLabel :: Parser Text
tokenLabel = "oauth_token:" *> extractText

tokenSecretLabel :: Parser Text
tokenSecretLabel = "oauth_token_secret:" *> extractText

configParser :: Parser Config
configParser = do
  en_ <- entryLabel <* endOfLine
  sname <- oauthServerNameLabel <* endOfLine
  ockey <- authConsumerKeyLabel <* endOfLine
  ocs <- oauthConsumerSecretLabel <* endOfLine
  tok <- tokenLabel <* endOfLine
  tos <- tokenSecretLabel

  let en = BS.unpack $ encodeUtf8 en_
      oa = newOAuth 
        {
          oauthServerName = unpack sname,
          oauthConsumerKey = encodeUtf8 ockey,
          oauthConsumerSecret = encodeUtf8 ocs
        }
      cre = newCredential (encodeUtf8 tok) (encodeUtf8 tos)
    in pure $ Config en oa cre


extract :: IO (Either String Config)
extract = parseOnly configParser <$> readFile configFile