module Tests.Event where

import           Data.GS1.BizStep
import           Data.GS1.BizTransaction
import           Data.GS1.Disposition
import           Data.GS1.Event
import           Data.GS1.URI
import           Test.Hspec

testBizStep :: Spec
testBizStep =
  describe "BusinessStep" $
    it "produces correct URI" $
      ppURI Accepting `shouldBe` "urn:epcglobal:cbv:bizstep:accepting"

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

