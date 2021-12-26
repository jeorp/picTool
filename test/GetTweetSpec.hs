{-# LANGUAGE OverloadedStrings #-}
module GetTweetSpec where

import Test.Hspec
import qualified Data.Text as T
import GetTweet

main :: IO () 
main = hspec spec

spec :: Spec
spec = do
  describe "Test GetTweet" $ do
    it "test 1" $ do
      execute "from:ngnchiikawa filter:images" `shouldReturn` ()

