module Tests.Event where

import           Test.Hspec

import           Data.GS1.BizTransaction
import           Data.GS1.DWhat
import           Data.GS1.DWhy
import           Data.GS1.EPC
import           Data.GS1.URI

testBizStep :: Spec
testBizStep = do
  describe "BusinessStep" $
    it "produces correct URI" $
      ppURI Accepting `shouldBe` "urn:epcglobal:cbv:bizstep:accepting"

  describe "parseBizStep" $ do
    it "parse valid uri to bizstep" $
      mkBizStep "urn:epcglobal:cbv:bizstep:void_shipping" `shouldBe` Just VoidShipping

    it "parse valid uri to bizstep" $
      mkBizStep "urn:epcglobal:cbv:bizstep:accepting" `shouldBe` Just Accepting

    it "parse valid uri but invalid step to Nothing" $
      mkBizStep "urn:epcglobal:cbv:bizstep:s" `shouldBe` Nothing

    it "parse invalid uri to Nothing" $
      mkBizStep "urn:invalidns:cbv:bizstep:void_shipping" `shouldBe` Nothing

testDisposition :: Spec
testDisposition = do
  describe "Disposition" $
    it "produces correct URI" $
      ppURI Active `shouldBe` "urn:epcglobal:cbv:disp:active"

  describe "parse Disposition" $ do
    it "parse the valid uri to disposition" $
      mkDisposition "urn:epcglobal:cbv:disp:active" `shouldBe` Just Active
    it "parses the invalid uri to Nothing" $
      mkDisposition "urn:epcglobal:cbv:disp:active2" `shouldBe` Nothing
    it "parse invalid string to Nothing" $
      mkDisposition "somerandomstring" `shouldBe` Nothing
    it "parse invalid string to Nothing" $
      mkDisposition "" `shouldBe` Nothing

testBizTransaction :: Spec
testBizTransaction =
  describe "Parse BizTransactionID" $ do
    it "parse the valid uri to BizTransactionID" $
      parseBizTransactionType "urn:epcglobal:cbv:btt:po" `shouldBe` Just Po
    it "parse the invalid uri to Nothing" $
      parseBizTransactionType "urn:epcglobal:cbv:btt:somethingelse" `shouldBe` Nothing
    it "parse the empty uri to Nothing" $
      parseBizTransactionType "" `shouldBe` Nothing

testMkDWhat :: Spec
testMkDWhat = do
  describe "create valid ObjectDWhat" $
    it "creates DWhat from valid input" $
      ppDWhat (ObjectDWhat Observe [GLN "urn:epc:id:sgtin:0614141" "107346" "2017", GLN "urn:epc:id:sgtin:0614141" "107346" "2018"] []) `shouldBe` "OBJECT WHAT\nObserve\n[urn:epc:id:sgtin:0614141.107346.2017,urn:epc:id:sgtin:0614141.107346.2018]\n[]"
  describe "create from empty epcs" $
    it "creates DWhat from empty epc list" $
      ppDWhat (ObjectDWhat Add [][]) `shouldBe` "OBJECT WHAT\nAdd\n[]\n[]"
