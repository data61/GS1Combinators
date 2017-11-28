{-# LANGUAGE DataKinds #-}

module Tests.DWhat where

import           Data.Maybe
import           Test.Hspec

import           Data.GS1.DWhat
import           Data.GS1.EPC
import           Data.GS1.Object

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

testBizTransaction :: Spec
testBizTransaction = do
  describe "Parse BizTransactionID" $
    it "parse the valid uri to BizTransactionID" $
      readURI "urn:epcglobal:cbv:btt:po" `shouldBe` Just Po

  describe "Invalid urns" $ do
    it "parse the invalid uri to Nothing" $
      (readURI :: String -> Maybe BizTransactionType) "urn:epcglobal:cbv:btt:somethingelse"
        `shouldBe` Nothing
    it "parse the empty uri to Nothing" $
      (readURI :: String -> Maybe BizTransactionType) "" `shouldBe` Nothing
    it "parse a rubbish uri with : to Nothing" $
      (readURI :: String -> Maybe BizTransactionType) "urn:epcglobal:cbv:foo" `shouldBe` Nothing
    it "parse a rubbish uri with no : to Nothing" $
      (readURI :: String -> Maybe BizTransactionType) "fooblahjaja" `shouldBe` Nothing

-- TODO test for quantity
testPpDWhat :: Spec
testPpDWhat = do
  describe "create valid ObjectDWhat" $
    it "creates ObjectDWhat from valid input" $
      ppDWhat (ObjectDWhat Observe [IL (SGTIN "0614141" Nothing "107346" "2017"),
        IL (SGTIN "0614141" Nothing "107346" "2018")] [])
          `shouldBe`
            "OBJECT WHAT\nObserve\n" ++
            "[IL (SGTIN \"0614141\" Nothing \"107346\" \"2017\")," ++
            "IL (SGTIN \"0614141\" Nothing \"107346\" \"2018\")]\n[]\n"
            --"OBJECT WHAT\nObserve\n[urn:epc:id:sgtin:0614141.107346.2017,urn:epc:id:sgtin:0614141.107346.2018]\n[]"

  describe "create from empty epcs" $
    it "creates DWhat from empty epc list" $
      ppDWhat (ObjectDWhat Add [][]) `shouldBe` "OBJECT WHAT\nAdd\n[]\n[]\n"

  describe "create valid AggregationDWhat" $
    it "creates AggregationDWhat from valid input" $
      ppDWhat (AggregationDWhat Observe 
        (Just (IL (SSCC "0614141" "1234567890")))
        [IL (SGTIN "0614141" Nothing "107346" "2017"),
        IL (SGTIN "0614141" Nothing "107346" "2018")])
          `shouldBe`
            "AGGREGATION WHAT\nObserve\nJust (IL (SSCC \"0614141\" \"1234567890\"))\n" ++
            "[IL (SGTIN \"0614141\" Nothing \"107346\" \"2017\")," ++
            "IL (SGTIN \"0614141\" Nothing \"107346\" \"2018\")]\n"

          -- DELETEME
          --"AGGREGATION WHAT\n" ++ "Observe\n" ++
          --"Just \"urn:epc:id:sscc:0614141.1234567890\"\n" ++ 
          --"[urn:epc:id:sgtin:0614141.107346.2017," ++
          --"urn:epc:id:sgtin:0614141.107346.2018]\n" ++
          --"[QuantityElement (EPCClass \"urn:epc:idpat:sgtin:4012345.098765.*\") 10.0 Nothing," ++
          --"QuantityElement (EPCClass \"urn:epc:class:lgtin:4012345.012345.998877\") 200.5 (Just \"KGM\")]"
