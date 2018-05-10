{-# LANGUAGE TypeApplications #-}

module Tests.DWhy where

import           Data.GS1.EPC
import           Test.Hspec

testDisposition :: Spec
testDisposition =
  describe "Disposition" $ do
    describe "Print URI" $ do
      it "One word" $
        renderURL Active `shouldBe` "urn:epcglobal:cbv:disp:active"
      it "Multiple words" $
        renderURL ContainerClosed `shouldBe` "urn:epcglobal:cbv:disp:container_closed"

    describe "parse Disposition where invalid" $ do
      it "parse the valid uri to disposition" $
        readURI "urn:epcglobal:cbv:disp:active" `shouldBe` Right Active
      it "parses the invalid uri to Nothing" $
        readURI @Disposition
          "urn:epcglobal:cbv:disp:active2"
            `shouldBe`
              (Left $ InvalidFormat (XMLSnippet "urn:epcglobal:cbv:disp:active2"))
      it "parse invalid string to Nothing" $
        readURI @Disposition
          "somerandomstring"
            `shouldBe`
              (Left $ InvalidFormat (XMLSnippet "somerandomstring"))
      it "parse empty string to Nothing" $
        readURI @Disposition
          ""
            `shouldBe`
              (Left $ InvalidFormat (XMLSnippet ""))
