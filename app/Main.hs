{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString as B

--import Data.Extensible.GetOpt
import Control.Concurrent.Async
import Data.Either
import Data.Aeson
import Data.Aeson.Lens
--import Data.Aeson.Encode.Pretty as P
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS 
import qualified Data.Vector as V
import Data.Maybe
import System.Directory
import Control.Lens
import GetToken
import Common
import Download
import Twitter.GetTweet
import Control.Monad.Reader


type App = ReaderT Config IO

run :: App () -> IO ()
run app = do
  config <- extract
  let init = fromRight defaultConfig config
  runReaderT app init

searchAndStoreApp :: T.Text -> App ()
searchAndStoreApp text = do
  e <- entitiedSearch text :: App (Either String Searched)
  case e of
    Left l -> liftIO $ print l
    Right r -> do
      let vals = fromMaybe V.empty $ statuses r ^? _Array
          urls =  collectMediaUrls vals
      config <- ask 
      liftIO $ do
        putStrLn $ "Start donwload ..\n" <>  show (V.length urls) <> " puctures ..., please wait"
        mapConcurrently_ (storeFromUrl $ config ^. temp) urls
        putStrLn "\nfinish"


app :: App ()
app = do
  liftIO $ putStrLn "Please input word - ex: クリスマス"
  s <- liftIO getLine
  config <- ask
  liftIO $ doesDirectoryExist (config ^. temp) >>= flip unless (createDirectory $ config ^. temp)
  searchAndStoreApp $ T.pack s

main :: IO ()
main = run app