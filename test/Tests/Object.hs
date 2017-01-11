module Tests.Object where

import           Data.GS1.Object
import           Test.Hspec

testObjectID :: Spec
testObjectID = do
  describe "RFC 3986 segment nz" $ do
    it "returns the ObjectID from valid string" $
      httpObjectID "data61//chain.supply.!@#$%^&*()-_=+[]{}:;',.<>?/random.string" `shouldBe` Just "data61"

    it "returns Nothing on invalid string" $
      httpObjectID "data61.chain.supply.this.string.is.invalud" `shouldBe` Nothing

  describe "validate ObjectID" $ do
    it "injects valid object ID to Just" $
      validateObjectID "ASimpleID" `shouldBe` Just "ASimpleID"

    it "empty string is not valid" $
      validateObjectID "" `shouldBe` Nothing

    it "recognises string mixed with good hexDigit" $
      validateObjectID "%10abcdod%1a%2F" `shouldBe` Just "%10abcdod%1a%2F"

    it "recognises string mixed with bad hexDigit" $
      validateObjectID "%1zabcdod" `shouldBe` Nothing

    it "recognises all the unreserved chars" $
      validateObjectID unreserved `shouldBe` Just unreserved

    it "recognises all the subdelims" $
      validateObjectID subdelims `shouldBe` Just subdelims

    it "returns False when bad char occurs" $
      validSegmentNzChar <$> "/?<>{}[]%^`|" `shouldBe` replicate 12 False

