module Tests.DWhy where

import Test.Hspec

import Data.GS1.DWhy
import Data.GS1.URI
import Data.GS1.EPC

testBizStep :: Spec
testBizStep = do
  describe "BusinessStep" $
    it "produces correct URI" $
      printURI Accepting `shouldBe` "urn:epcglobal:cbv:bizstep:accepting"

  describe "parseBizStep" $ do
    it "parse valid uri to bizstep" $
      readURI "urn:epcglobal:cbv:bizstep:void_shipping" `shouldBe` Just VoidShipping

    it "parse valid uri to bizstep" $
      readURI "urn:epcglobal:cbv:bizstep:accepting" `shouldBe` Just Accepting

    it "parse valid uri but invalid step to Nothing" $
      (readURI :: String -> Maybe BizStep) "urn:epcglobal:cbv:bizstep:s" `shouldBe` Nothing

    it "parse invalid uri to Nothing" $
      (readURI :: String -> Maybe BizStep) "urn:invalidns:cbv:bizstep:void_shipping" `shouldBe` Nothing

testDisposition :: Spec
testDisposition = do
  describe "Disposition" $
    it "produces correct URI" $
      printURI Active `shouldBe` "urn:epcglobal:cbv:disp:active"

  describe "parse Disposition" $ do
    it "parse the valid uri to disposition" $
      readURI "urn:epcglobal:cbv:disp:active" `shouldBe` Just Active
    it "parses the invalid uri to Nothing" $
      (readURI :: String -> Maybe Disposition) "urn:epcglobal:cbv:disp:active2" `shouldBe` Nothing
    it "parse invalid string to Nothing" $
      (readURI :: String -> Maybe Disposition) "somerandomstring" `shouldBe` Nothing
    it "parse invalid string to Nothing" $
      (readURI :: String -> Maybe Disposition) "" `shouldBe` Nothing
