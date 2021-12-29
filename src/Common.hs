
module Common where

import Data.Text
import Web.Authenticate.OAuth

configFile :: String
configFile = "config.env"

data Config = Config 
  {
    oatuh_ :: OAuth,
    credential_ :: Credential
  }