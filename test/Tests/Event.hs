module Tests.Event where

import           Data.Either.Combinators
import           Data.GS1.BizStep
import           Data.GS1.BizTransaction
import           Data.GS1.Disposition
import           Data.GS1.DWhat
import           Data.GS1.EPC
import           Data.GS1.EPCISTime
import           Data.GS1.Event
import           Data.GS1.URI
import           Data.GS1.Utils
import           Data.Time.LocalTime
import           Test.Hspec

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

  -- TODO test create other DWhat too

testMkDWhen :: Spec
testMkDWhen = do
  describe "create valid DWhen" $ do
    it "creates DWhen from two valid time Strngs" $ do
      let et = "2005-04-03T20:33:31.116-06:00"
      let rt = "2005-04-03T20:39:47.16-06:00"

      let pet = fromRight' (parseStr2Time et :: Either EPCISTimeError EPCISTime)
      let prt = fromRight' (parseStr2Time rt :: Either EPCISTimeError EPCISTime)
      let tz = fromRight' (parseStr2TimeZone et :: Either EPCISTimeError TimeZone)
      let dwhen = DWhen pet (Just prt) tz

      mkDWhen et rt `shouldBe` Just dwhen
    
    it "event time should be smaller than record time" $ do
      let et = "2005-04-03T20:39:47.16-06:00"
      let rt = "2005-04-03T20:33:31.116-06:00"

      mkDWhen et rt `shouldBe` Nothing

    it "ignores the record time" $ do
      let et = "2005-04-03T20:33:31.116-06:00"
      let rt = "2005-04-03T20:39:47.16-06:00-invalid-format"

      let pet = fromRight' (parseStr2Time et :: Either EPCISTimeError EPCISTime)
      let tz = fromRight' (parseStr2TimeZone et :: Either EPCISTimeError TimeZone)
      let dwhen = DWhen pet Nothing tz

      mkDWhen et rt `shouldBe` Just dwhen

    it "does not ignore invalid event time" $ do
      let et = "2005-04-3T20:39:47.16-06:00-invalid-format"
      let rt = "2005-04-3T20:33:31.116-06:00"

      mkDWhen et rt `shouldBe` Nothing
  
  describe "create valid DWhen by mkDWhen'" $ do
    it "creates valid DWhen from one time string" $ do
      let t = "2017-01-24T13:08:24.11+10:00"
      let pt = fromRight' (parseStr2Time t :: Either EPCISTimeError EPCISTime)
      let tz = fromRight' (parseStr2TimeZone t :: Either EPCISTimeError TimeZone)

      let dwhen = DWhen pt (Just pt) tz

      mkDWhen' t `shouldBe` Just dwhen
    
    it "fails on invalid string" $ do
      let t = "invalid input"
      mkDWhen' t `shouldBe` Nothing
