module Tests.Event where

import           Data.GS1.BizStep
import           Data.GS1.BizTransaction
import           Data.GS1.Disposition
import           Data.GS1.Event
import Data.GS1.EPC
import Data.GS1.DWhat
import           Data.GS1.URI
import           Test.Hspec
import Control.Exception

testBizStep :: Spec
testBizStep =
  describe "BusinessStep" $
    it "produces correct URI" $
      ppURI Accepting `shouldBe` "urn:epcglobal:cbv:bizstep:accepting"


testParseBizStep :: Spec
testParseBizStep =
  describe "parseBizStep" $ do
    it "parse valid uri to bizstep" $
      parseBizStep "urn:epcglobal:cbv:bizstep:void_shipping" `shouldBe` Just VoidShipping

    it "parse valid uri to bizstep" $
      parseBizStep "urn:epcglobal:cbv:bizstep:accepting" `shouldBe` Just Accepting

    it "parse valid uri but invalid step to Nothing" $
      parseBizStep "urn:epcglobal:cbv:bizstep:s" `shouldBe` Nothing

    it "parse invalid uri to Nothing" $
      parseBizStep "urn:invalidns:cbv:bizstep:void_shipping" `shouldBe` Nothing

testDisposition :: Spec
testDisposition =
  describe "Disposition" $
    it "produces correct URI" $
      ppURI Active `shouldBe` "urn:epcglobal:cbv:disp:active"

testBizTransaction :: Spec
testBizTransaction = do
  describe "BizTransaction Id GDTI" $
    it "produces correct GDTI URI" $
      ppURI (BTIGDTI "dummyid") `shouldBe` "urn:epc:id:gdti:dummyid"

  describe "BizTransaction Id GSRN" $
    it "produces correct GSRN URI" $
      ppURI (BTIGSRN "dummyid") `shouldBe` "urn:epc:id:gsrn:dummyid"

  describe "BizTransaction Id GLN" $
    it "produces correct legacy GLN URI" $
      ppURI (BTIGLN "dummyid") `shouldBe` "urn:epcglobal:cbv:bt:gln:dummyid"

testCreateDWhat :: Spec
testCreateDWhat = do
  describe "create valid ObjectDWhat" $
    it "creates DWhat from valid input" $
      ppDWhat (ObjectDWhat Observe [GLN "urn:epc:id:sgtin:0614141" "107346" "2017", GLN "urn:epc:id:sgtin:0614141" "107346" "2018"] []) `shouldBe` "OBJECT WHAT\nObserve\n[urn:epc:id:sgtin:0614141.107346.2017,urn:epc:id:sgtin:0614141.107346.2018]\n[]"
  describe "create from empty epcs" $
    it "creates DWhat from empty epc list" $
      ppDWhat (ObjectDWhat Add [][]) `shouldBe` "OBJECT WHAT\nAdd\n[]\n[]"

  -- TODO test create other DWhat too

