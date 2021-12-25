module GetTweetSpec where

import Test.Hspec

import Download

main :: IO () 
main = hspec spec

spec :: Spec
spec = do
  describe "Test GetTweet" $ do
    it "test 1" $ do
      pure () `shouldReturn` ()