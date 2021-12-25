module DownloadSpec where

import Test.Hspec

main :: IO () 
main = hspec spec

spec :: Spec
spec = do
  describe "Test Download picture" $ do
    it "" $ do
      pure "" `shouldReturn` "" 