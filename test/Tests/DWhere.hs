module Tests.DWhere where

import           Data.GS1.EPC
import           Data.GS1.DWhere
import           Test.Hspec

testReadSGLN :: Spec
testReadSGLN =
  describe "Location - Testing readURI" $
    describe "SGLN with location reference" $ do
      it "LocationReferenceNum with extension" $
        readURI "urn:epc:id:sgln:0614141.12345.400" `shouldBe` 
          Right (SGLN "0614141" (LocationReferenceNum "12345") (Just "400"))
      it "LocationReferenceNum without extension" $
        readURI "urn:epc:id:sgln:0614141.12345" `shouldBe`
          Right (SGLN "0614141" (LocationReferenceNum "12345") Nothing)
      it "Invalid URI" $
        (readURI :: String -> Either ParseFailure LocationEPC)
          "this:is:invalid"
            `shouldBe` Left InvalidFormat
      it "Some other valid uri" $
        (readURI :: String -> Either ParseFailure LocationEPC)
          "urn:epc:class:lgtin:4012345.012345.998877"
            `shouldBe` Left InvalidFormat
      it "Empty string" $
        (readURI :: String -> Either ParseFailure LocationEPC) ""
          `shouldBe` Left InvalidFormat
      it "Some components missing" $
        (readURI :: String -> Either ParseFailure LocationEPC)
          "urn:epc:sgln:0614141.12345.400" `shouldBe` Left InvalidFormat
      describe "Invalid length" $ do
        it "Shorter length" $
          (readURI :: String -> Either ParseFailure LocationEPC)
            "urn:epc:id:sgln:06.12.4" `shouldBe` Left InvalidLength
        it "Longer length" $
          (readURI :: String -> Either ParseFailure LocationEPC)
            "urn:epc:id:sgln:06534590.123234322.4" `shouldBe` Left InvalidLength

testPrintSGLN :: Spec
testPrintSGLN =
  describe "Location - Testing printURI" $
    describe "SGLN with location reference" $ do
      it "LocationReferenceNum with extension" $
        printURI (SGLN "0614141" (LocationReferenceNum "12345") (Just "400"))
          `shouldBe` "urn:epc:id:sgln:0614141.12345.400"
      it "LocationReferenceNum without extension" $
        printURI (SGLN "0614141" (LocationReferenceNum "12345") Nothing)
          `shouldBe` "urn:epc:id:sgln:0614141.12345"
