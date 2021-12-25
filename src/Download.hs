{-# LANGUAGE OverloadedStrings #-}

module Download where
import Network.HTTP.Simple
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import System.IO
import System.Directory
import Data.Strings

-- input url and path 
downloadPic :: String -> String -> IO ()
downloadPic url path = do
  let outFile = "temp/" ++ path
  res <- httpBS $ parseRequest_ url
  let xs = getResponseHeader "Content-Type" res
      file = getResponseBody res
      contentType = if not (null xs) then head xs else ""
  print contentType
  store outFile file contentType
  where
    store :: FilePath -> B.ByteString -> B.ByteString -> IO ()
    store path bs c = do
      if B.isInfixOf "image/" c 
        then do 
          let picType = BS.unpack $ snd $ B.splitAt (B.length "image/") c 
          fin <- openBinaryFile (path<>"."<>picType) WriteMode
          hPutStr fin (BS.unpack bs)
          hClose fin
        else putStrLn "not picture file"


urlToFileName :: String -> String
urlToFileName s = 
  let xs = strSplitAll "/" s
      ls = if null xs then "error" else last xs
    in ls
