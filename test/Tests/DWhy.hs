module Tests.DWhy where

import Test.Hspec

import Data.GS1.DWhy
import Data.GS1.URI
import Data.GS1.EPC

testBizStep :: Spec
testBizStep = do
  describe "BusinessStep" $ do
    it "produces correct URI" $
      printURI Accepting `shouldBe` "urn:epcglobal:cbv:bizstep:accepting"
    it "produces correct URI" $
      printURI CycleCounting `shouldBe` "urn:epcglobal:cbv:bizstep:cycle_counting"
    it "produces correct URI" $
      printURI CreatingClassInstance `shouldBe` "urn:epcglobal:cbv:bizstep:creating_class_instance"
  
  describe "parseBizStep" $ do
    it "parse valid uri to bizstep" $
      readURI "urn:epcglobal:cbv:bizstep:void_shipping" `shouldBe` Just VoidShipping

    it "parse valid uri to bizstep" $
      readURI "urn:epcglobal:cbv:bizstep:accepting" `shouldBe` Just Accepting
  
  describe "Invalid urns" $ do
    it "parse valid uri but invalid step to Nothing" $
      (readURI :: String -> Maybe BizStep) "urn:epcglobal:cbv:bizstep:s" `shouldBe` Nothing

    it "parse invalid uri to Nothing" $
      (readURI :: String -> Maybe BizStep) "urn:invalidns:cbv:bizstep:void_shipping"
        `shouldBe` Nothing
    it "A component of the urn missing" $
      (readURI :: String -> Maybe BizStep) "urn:epc:cbv:bizstep"
        `shouldBe` Nothing

testDisposition :: Spec
testDisposition =
  describe "Disposition" $ do
    describe "Print URI" $ do
      it "One word" $
        printURI Active `shouldBe` "urn:epcglobal:cbv:disp:active"
      it "Multiple words" $
        printURI ContainerClosed `shouldBe` "urn:epcglobal:cbv:disp:container_closed"

    describe "parse  Disposition" $ do
      it "parse the valid uri to disposition" $
        readURI "urn:epcglobal:cbv:disp:active" `shouldBe` Just Active
      it "parses the invalid uri to Nothing" $
        (readURI :: String -> Maybe Disposition) "urn:epcglobal:cbv:disp:active2" `shouldBe` Nothing
      it "parse invalid string to Nothing" $
        (readURI :: String -> Maybe Disposition) "somerandomstring" `shouldBe` Nothing
      it "parse empty string to Nothing" $
        (readURI :: String -> Maybe Disposition) "" `shouldBe` Nothing
