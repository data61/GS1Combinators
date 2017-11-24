{-# LANGUAGE DataKinds #-}

module Tests.DWhat where

-- import           Data.Maybe
-- import           Test.Hspec

-- import           Data.GS1.DWhat
-- import           Data.GS1.EPC
-- import           Data.GS1.Object

-- testBizTransaction :: Spec
-- testBizTransaction =
--   describe "Parse BizTransactionID" $ do
--     it "parse the valid uri to BizTransactionID" $
--       parseBizTransactionType "urn:epcglobal:cbv:btt:po" `shouldBe` Just Po
--     it "parse the invalid uri to Nothing" $
--       parseBizTransactionType "urn:epcglobal:cbv:btt:somethingelse" `shouldBe` Nothing
--     it "parse the empty uri to Nothing" $
--       parseBizTransactionType "" `shouldBe` Nothing

-- testMkDWhat :: Spec
-- testMkDWhat = do
--   describe "create valid ObjectDWhat" $
--     it "creates ObjectDWhat from valid input" $
--       ppDWhat (ObjectDWhat Observe [GLN "urn:epc:id:sgtin:0614141" "107346" "2017", GLN "urn:epc:id:sgtin:0614141" "107346" "2018"] []) `shouldBe` "OBJECT WHAT\nObserve\n[urn:epc:id:sgtin:0614141.107346.2017,urn:epc:id:sgtin:0614141.107346.2018]\n[]"

--   describe "create from empty epcs" $
--     it "creates DWhat from empty epc list" $
--       ppDWhat (ObjectDWhat Add [][]) `shouldBe` "OBJECT WHAT\nAdd\n[]\n[]"

--   describe "create valid AggregationDWhat" $
--     it "creates AggregationDWhat from valid input" $
--       ppDWhat (AggregationDWhat Observe (Just "urn:epc:id:sscc:0614141.1234567890") [EPC "rn:epc:id:sgtin:0614141.107346.2017", EPC "urn:epc:id:sgtin:0614141.107346.2018"] [QuantityElement (EPCClass "urn:epc:idpat:sgtin:4012345.098765.*") (10 :: Double) Nothing, QuantityElement (EPCClass "urn:epc:class:lgtin:4012345.012345.998877") (200.5 :: Double) (Just "KGM")]) `shouldBe` "AGGREGATION WHAT\nObserve\nJust \"urn:epc:id:sscc:0614141.1234567890\"\n[rn:epc:id:sgtin:0614141.107346.2017,urn:epc:id:sgtin:0614141.107346.2018]\n[QuantityElement (EPCClass \"urn:epc:idpat:sgtin:4012345.098765.*\") 10.0 Nothing,QuantityElement (EPCClass \"urn:epc:class:lgtin:4012345.012345.998877\") 200.5 (Just \"KGM\")]"
