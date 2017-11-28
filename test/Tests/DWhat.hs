{-# LANGUAGE DataKinds #-}

module Tests.DWhat where

import           Data.Maybe
import           Test.Hspec

import           Data.GS1.DWhat
import           Data.GS1.EPC
import           Data.GS1.Object

testBizTransaction :: Spec
testBizTransaction =
  describe "Parse BizTransactionID" $ do
    it "parse the valid uri to BizTransactionID" $
      readURI "urn:epcglobal:cbv:btt:po" `shouldBe` Just Po
    it "parse the invalid uri to Nothing" $
      (readURI :: String -> Maybe BizTransactionType) "urn:epcglobal:cbv:btt:somethingelse"
        `shouldBe` Nothing
    it "parse the empty uri to Nothing" $
      (readURI :: String -> Maybe BizTransactionType) "" `shouldBe` Nothing

-- TODO = FIXME. commented following out because stops from compiling

testMkDWhat :: Spec
testMkDWhat = do
  describe "create valid ObjectDWhat" $
    it "creates ObjectDWhat from valid input" $
      ppDWhat (ObjectDWhat Observe [IL (SGTIN "0614141" Nothing "107346" "2017"),
        IL (SGTIN "0614141" Nothing "107346" "2018")] [])
          `shouldBe`
            "OBJECT WHAT\nObserve\n[urn:epc:id:sgtin:0614141.107346.2017,urn:epc:id:sgtin:0614141.107346.2018]\n[]"

  describe "create from empty epcs" $
    it "creates DWhat from empty epc list" $
      ppDWhat (ObjectDWhat Add [][]) `shouldBe` "OBJECT WHAT\nAdd\n[]\n[]"

  describe "create valid AggregationDWhat" $
    it "creates AggregationDWhat from valid input" $
      ppDWhat (AggregationDWhat Observe 
        (Just (IL (SSCC "0614141" "1234567890")))
        [IL (SGTIN "0614141" Nothing "107346" "2017"),
        IL (SGTIN "0614141" Nothing "107346" "2018")])
        `shouldBe`
          "AGGREGATION WHAT\n" ++ "Observe\n" ++
          "Just \"urn:epc:id:sscc:0614141.1234567890\"\n" ++ 
          "[urn:epc:id:sgtin:0614141.107346.2017," ++
          "urn:epc:id:sgtin:0614141.107346.2018]\n" ++
          "[QuantityElement (EPCClass \"urn:epc:idpat:sgtin:4012345.098765.*\") 10.0 Nothing," ++
          "QuantityElement (EPCClass \"urn:epc:class:lgtin:4012345.012345.998877\") 200.5 (Just \"KGM\")]"
