module Tests.Location where

import Test.Hspec
import Data.GS1.Location

testPassLocId :: Spec
testPassLocId = do
  describe "Location" $ do
    it "loc id is passed correctly" $
      getLocId (ReadPointLocation "934858501.001.xxx") `shouldBe` "934858501.001.xxx"
