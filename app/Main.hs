{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString as B

--import Data.Extensible.GetOpt
import Data.Either
import Data.Aeson
import Data.Aeson.Lens
--import Data.Aeson.Encode.Pretty as P
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS 
import qualified Data.Vector as V
import Data.Maybe
import Control.Lens hiding ((:>))
import GetToken
import Common
import Download
import Twitter.GetTweet
import Twitter.Record
import Control.Monad.Reader


type Ap = ReaderT Config IO

run :: Ap () -> IO ()
run app = do
  config <- extract
  let init = fromRight defaultConfig config
  runReaderT app init

searchAndStoreApp :: T.Text -> Ap ()
searchAndStoreApp text = do
  e <- entitiedSearch text :: Ap (Either String Searched)
  case e of
    Left l -> liftIO $ print l
    Right r -> do
      let vals = fromMaybe V.empty $ statuses r ^? _Array
          urls =  collectMediaUrls vals
      liftIO $ mapM_ storeFromUrl urls

app :: Ap ()
app = do
  config <- ask
  liftIO $ print $ config ^. credentiall 
  searchAndStoreApp "声優"

main :: IO ()
main = run app