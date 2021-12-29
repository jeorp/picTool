module GetTokenSpec where

import Test.Hspec
import Data.Either
import GetToken

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Test GetToken" $ do
    it "fist test" $ do
      isRight <$> extract `shouldReturn` True