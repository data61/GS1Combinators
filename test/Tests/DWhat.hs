{-# LANGUAGE DataKinds #-}

module Tests.DWhat where

-- import           Data.Maybe
import           Data.GS1.DWhat
import           Data.GS1.EPC
import qualified Data.Text      as T
import           Test.Hspec

testLabelEPC :: Spec
testLabelEPC =
  describe "Agnostic readLabelEPC -> urn2LabelEPC" $ do
    -- since SGTIN can be both Instance and Class
    describe "SGTINs" $ do
      it "Instance SGTIN" $
        urn2LabelEPC "urn:epc:id:sgtin:0614141.107346.2017"
          `shouldBe`
            (Right $ IL $ SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017"))
      it "Class SGTIN" $
        urn2LabelEPC "urn:epc:idpat:sgtin:4012345.098765.*"
          `shouldBe`
            (Right $ CL (CSGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "098765")) Nothing)
    describe "Non SGTINs" $ do
      it "Instance SSCC" $
        urn2LabelEPC "urn:epc:id:sscc:0614141.1234567890"
          `shouldBe`
            (Right $ IL $ SSCC (GS1CompanyPrefix "0614141") (SerialNumber "1234567890"))
      it "Class LGTIN" $
        urn2LabelEPC "urn:epc:class:lgtin:4012345.012345.998877"
          `shouldBe`
            (Right $ CL (LGTIN (GS1CompanyPrefix "4012345") (ItemReference "012345") (Lot "998877")) Nothing)
testBizStep :: Spec
testBizStep = do
  describe "BusinessStep" $ do
    it "produces correct URI" $
      renderURL Accepting `shouldBe` "urn:epcglobal:cbv:bizstep:accepting"
    it "produces correct URI" $
      renderURL CycleCounting `shouldBe`
        "urn:epcglobal:cbv:bizstep:cycle_counting"
    it "produces correct URI" $
      renderURL CreatingClassInstance `shouldBe`
        "urn:epcglobal:cbv:bizstep:creating_class_instance"

  describe "parseBizStep" $ do
    it "parse valid uri to bizstep" $
      readURI "urn:epcglobal:cbv:bizstep:void_shipping" `shouldBe`
        Right VoidShipping

    it "parse valid uri to bizstep" $
      readURI "urn:epcglobal:cbv:bizstep:accepting" `shouldBe` Right Accepting

  describe "Invalid urns" $ do
    it "parse valid uri but invalid step to Nothing" $
      (readURI :: T.Text -> Either ParseFailure BizStep)
        "urn:epcglobal:cbv:bizstep:s"
          `shouldBe` (Left $ InvalidFormat (XMLSnippet "urn:epcglobal:cbv:bizstep:s"))
    it "parse invalid uri to Nothing" $
      (readURI :: T.Text -> Either ParseFailure BizStep)
        "urn:invalidns:cbv:bizstep:void_shipping"
          `shouldBe` (Left $ InvalidFormat (XMLSnippet "urn:invalidns:cbv:bizstep:void_shipping"))
    it "A component of the urn missing" $
      (readURI :: T.Text -> Either ParseFailure BizStep) "urn:epc:cbv:bizstep"
        `shouldBe` (Left $ InvalidFormat (XMLSnippet "urn:epc:cbv:bizstep"))
    it "empty" $
      (readURI :: T.Text -> Either ParseFailure BizStep) ""
        `shouldBe` (Left $ InvalidFormat (XMLSnippet ""))

testBizTransaction :: Spec
testBizTransaction = do
  describe "Parse BizTransactionID" $
    it "parse the valid uri to BizTransactionID" $
      readURI "urn:epcglobal:cbv:btt:po" `shouldBe` Right Po

  describe "Invalid urns" $ do
    it "parse the invalid uri" $
      (readURI :: T.Text -> Either ParseFailure BizTransactionType)
        "urn:epcglobal:cbv:btt:somethingelse"
          `shouldBe` (Left $ InvalidFormat (XMLSnippet "urn:epcglobal:cbv:btt:somethingelse"))
    it "empty uri" $
      (readURI :: T.Text -> Either ParseFailure BizTransactionType)
        "" `shouldBe` (Left $ InvalidFormat (XMLSnippet ""))
    it "parse a uri missing component" $
      (readURI :: T.Text -> Either ParseFailure BizTransactionType)
        "urn:epcglobal:cbv:po"
          `shouldBe` (Left $ InvalidFormat (XMLSnippet "urn:epcglobal:cbv:po"))
    it "parse a rubbish uri with no" $
      (readURI :: T.Text -> Either ParseFailure BizTransactionType)
        "fooblahjaja" `shouldBe` (Left $ InvalidFormat (XMLSnippet "fooblahjaja"))

  describe "print BizTransaction" $ do
    it "print BizTransaction 1" $
      renderURL Bol `shouldBe` "urn:epcglobal:cbv:btt:bol"
    it "print BizTransaction 2" $
      renderURL Prodorder `shouldBe` "urn:epcglobal:cbv:btt:prodorder"
