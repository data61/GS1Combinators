module Tests.DWhat where

import           Test.Hspec

import           Data.GS1.BizTransaction
import           Data.GS1.DWhat
import           Data.GS1.EPC

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
