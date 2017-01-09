module Tests.Object where

import           Data.GS1.Object
import           Test.Hspec

testHttpObjectID :: Spec
testHttpObjectID =
  describe "RFC 3986 segment nz" $ do
    it "returns the ObjectID from valid string" $
      httpObjectID "data61//chain.supply.!@#$%^&*()-_=+[]{}:;',.<>?/random.string" `shouldBe` Just "data61"

    it "returns Nothing on invalid string" $
      httpObjectID "data61.chain.supply.this.string.is.invalud" `shouldBe` Nothing

