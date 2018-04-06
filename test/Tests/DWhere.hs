{-# LANGUAGE OverloadedStrings #-}
module Tests.DWhere where

import           Data.GS1.EPC
import qualified Data.Text    as T
import           Test.Hspec

testReadSGLN :: Spec
testReadSGLN =
  describe "Location - Testing readURI" $
    describe "SGLN with location reference" $ do
      it "LocationReference with extension" $
        readURI "urn:epc:id:sgln:0614141.12345.400" `shouldBe`
          Right (SGLN (GS1CompanyPrefix "0614141") (LocationReference "12345") (Just (SGLNExtension "400")))
      it "LocationReference without extension" $
        readURI "urn:epc:id:sgln:0614141.12345" `shouldBe`
          Right (SGLN (GS1CompanyPrefix "0614141") (LocationReference "12345") Nothing)
      it "Invalid URI" $
        (readURI :: T.Text -> Either ParseFailure LocationEPC)
          "this:is:invalid"
            `shouldBe` Left InvalidFormat
      it "Some other valid uri" $
        (readURI :: T.Text -> Either ParseFailure LocationEPC)
          "urn:epc:class:lgtin:4012345.012345.998877"
            `shouldBe` Left InvalidFormat
      it "Empty string" $
        (readURI :: T.Text -> Either ParseFailure LocationEPC) ""
          `shouldBe` Left InvalidFormat
      it "Some components missing" $
        (readURI :: T.Text -> Either ParseFailure LocationEPC)
          "urn:epc:sgln:0614141.12345.400" `shouldBe` Left InvalidFormat
      describe "Invalid length" $ do
        it "Shorter length" $
          (readURI :: T.Text -> Either ParseFailure LocationEPC)
            "urn:epc:id:sgln:06.12.4" `shouldBe` Left InvalidLength
        it "Longer length" $
          (readURI :: T.Text -> Either ParseFailure LocationEPC)
            "urn:epc:id:sgln:06534590.123234322.4" `shouldBe` Left InvalidLength

testPrintSGLN :: Spec
testPrintSGLN =
  describe "Location - Testing renderURL" $
    describe "SGLN with location reference" $ do
      it "LocationReference with extension" $
        renderURL (SGLN (GS1CompanyPrefix "0614141") (LocationReference "12345") (Just (SGLNExtension "400")))
          `shouldBe` "urn:epc:id:sgln:0614141.12345.400"
      it "LocationReference without extension" $
        renderURL (SGLN (GS1CompanyPrefix "0614141") (LocationReference "12345") Nothing)
          `shouldBe` "urn:epc:id:sgln:0614141.12345"
