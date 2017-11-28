module Tests.DWhy where

import Test.Hspec

import Data.GS1.DWhy
import Data.GS1.URI
import Data.GS1.EPC

testDisposition :: Spec
testDisposition =
  describe "Disposition" $ do
    describe "Print URI" $ do
      it "One word" $
        printURI Active `shouldBe` "urn:epcglobal:cbv:disp:active"
      it "Multiple words" $
        printURI ContainerClosed `shouldBe` "urn:epcglobal:cbv:disp:container_closed"

    describe "parse Disposition where invalid" $ do
      it "parse the valid uri to disposition" $
        readURI "urn:epcglobal:cbv:disp:active" `shouldBe` Just Active
      it "parses the invalid uri to Nothing" $
        (readURI :: String -> Maybe Disposition) "urn:epcglobal:cbv:disp:active2" `shouldBe` Nothing
      it "parse invalid string to Nothing" $
        (readURI :: String -> Maybe Disposition) "somerandomstring" `shouldBe` Nothing
      it "parse empty string to Nothing" $
        (readURI :: String -> Maybe Disposition) "" `shouldBe` Nothing
