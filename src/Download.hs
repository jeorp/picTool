{-# LANGUAGE OverloadedStrings #-}

module Download where
import Network.HTTP.Simple

downloadPic :: Request -> IO ()
downloadPic url = do
  res <- httpLBS url
  print (getResponseBody res)
  print (getResponseHeaders res)