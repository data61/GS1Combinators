module Tests.Event where

import Test.Hspec
import Data.GS1.Event
import Data.GS1.URI

testBizStep :: Spec
testBizStep = do
  describe "BusinessStep" $ do
    it "produces correct URI" $
      ppURI Accepting `shouldBe` "urn:epcglobal:cbv:bizstep:accepting"
  
testDisposition :: Spec
testDisposition = do
  describe "Disposition" $ do
    it "produces correct URI" $
      ppURI Active `shouldBe` "urn:epcglobal:cbv:disp:active"

testBizTransaction :: Spec
testBizTransaction = do
  describe "BizTransaction Id GDTI" $ do
    it "produces correct GDTI URI" $
      ppURI (BTIGDTI "dummyid") `shouldBe` "urn:epc:id:gdti:dummyid"

  describe "BizTransaction Id GSRN" $ do
    it "produces correct GSRN URI" $
      ppURI (BTIGSRN "dummyid") `shouldBe` "urn:epc:id:gsrn:dummyid"

  describe "BizTransaction Id GLN" $ do
    it "produces correct legacy GLN URI" $
      ppURI (BTIGLN "dummyid") `shouldBe` "urn:epcglobal:cbv:bt:gln:dummyid"

