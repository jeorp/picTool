{-# LANGUAGE OverloadedStrings #-}

module Download where
import Network.HTTP.Simple
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import System.IO
import System.Directory
import Data.Strings

-- input url and path 
downloadPic :: String -> String -> IO ()
downloadPic url path = do
  let outFile = path
  b <- doesFileExist outFile
  unless b $ do
    res <- httpBS $ parseRequest_ url
    let xs = getResponseHeader "Content-Type" res
        file = getResponseBody res
        contentType = if not (null xs) then head xs else ""
    store outFile file contentType
  where
    store :: FilePath -> B.ByteString -> B.ByteString -> IO ()
    store path bs c = do
      if B.isInfixOf "image/" c 
        then do 
          let picType = BS.unpack $ snd $ B.splitAt (B.length "image/") c
              local = path
          fin <- openBinaryFile local WriteMode
          hPutStr fin (BS.unpack bs)
          hClose fin
        else putStrLn "not picture file"


urlToFileName :: String -> String
urlToFileName s = 
  let xs = strSplitAll "/" s
      ls = if null xs then "error" else last xs
    in ls

eliminate :: String -> String
eliminate s = 
  let xs = strSplitAll "." s
      ls = if null xs then s else head xs
    in ls

storeFromUrl :: String -> String -> IO ()
storeFromUrl tmp url = downloadPic url (tmp <> urlToFileName url)