module DownloadSpec where

import Test.Hspec

import Download

main :: IO () 
main = hspec spec

spec :: Spec
spec = do
  describe "Test Download picture" $ do
    it "test 1" $ do
      storeFromUrl "https://pbs.twimg.com/media/FHcf4AKaIAk5soi?format=jpg&name=large"  `shouldReturn` ()