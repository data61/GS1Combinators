{-# LANGUAGE OverloadedStrings #-}

module Tests.Parser where

import           Data.GS1.BizStep
import           Data.GS1.Disposition
import           Data.GS1.DWhat
import           Data.GS1.DWhy
import           Data.GS1.EPC
import           Data.GS1.Event
import           Data.GS1.Location
import           Data.GS1.Object
import           Data.Maybe
import           Test.Hspec
import           Text.XML
import           Text.XML.Cursor
import           XML.Parser

testParser :: Spec
testParser = do
  describe "parse XML to obtain DWhen" $ do
    it "creates DWhen from valid XML" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEvent.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "ObjectEvent" cursor
      --mapM_ print $ parseDWhen <$> oeCursors
      let et = "2005-04-03T20:33:31.116-06:00"
      let et1 = "2005-04-04T20:33:31.116-06:00"
      parseDWhen <$> oeCursors `shouldBe` [mkDWhen' et, mkDWhen et1 ""]

    it "creates Nothing from Single ObjectEvent XML without Event Time" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEventNoEventTime.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "ObjectEvent" cursor
      parseDWhen <$> oeCursors `shouldBe` [Nothing]

  describe "parse XML to obtain Action" $
    it "finds action from Single ObjectEvent XML" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEventNoEventTime.xml"
      let cursor = fromDocument doc
      let actions = cursor $// element "action" &/ content
      parseAction actions `shouldBe` Just Observe

  describe "parse XML to obtain EPC List" $
    it "finds all epcs" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEventNoEventTime.xml"
      let cursor = fromDocument doc
      let epcs = cursor $// element "epc" &/ content
      show <$> parseEPCList epcs `shouldBe` ["urn:epc:id:sgtin:0614141.107346.2017", "urn:epc:id:sgtin:0614141.107346.2018"]

  describe "parse XML to get BizStep" $ do
    it "find all the BizStep in multiple events XML" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEvent.xml"
      let cursor = fromDocument doc
      let bizStepText = cursor $// element "ObjectEvent" &/ element "bizStep" &/ content
      parseBizStep bizStepText `shouldBe` Just Shipping

    it "find the first BizStep in single Event XML" $ do
      doc2 <- Text.XML.readFile def "test/test-xml/ObjectEvent2.xml"
      let cursor = fromDocument doc2
      let bizStepText = cursor $// element "ObjectEvent" &/ element "bizStep" &/ content
      parseBizStep bizStepText `shouldBe` Just Shipping

  describe "parse XML to get Disposition" $
    it "find all the Disposition in single XML" $ do
      doc2 <- Text.XML.readFile def "test/test-xml/ObjectEvent2.xml"
      let cursor = fromDocument doc2
      let dispText = cursor $// element "ObjectEvent" &/ element "disposition" &/ content
      parseDisposition dispText `shouldBe` Just InTransit

  describe "parse DWhy" $
    it "find the bizStep or disposition and creates a DWhy" $ do
      doc2 <- Text.XML.readFile def "test/test-xml/ObjectEvent2.xml"
      let cursor = fromDocument doc2
      let oeCursors = cursor $// element "ObjectEvent"
      parseDWhy <$> oeCursors `shouldBe` [Just $ DWhy (Just Shipping) (Just InTransit)]

  describe "parse XML to obtain DWhere" $
    it "finds all the dwhere" $ do
      doc2 <- Text.XML.readFile def "test/test-xml/ObjectEvent.xml"
      let cursor = fromDocument doc2
      let oeCursors = getCursorsByName "ObjectEvent" cursor
      --mapM_ print $ parseDWhere <$> oeCursors
      parseDWhere <$> oeCursors `shouldBe` [Just DWhere {
                                                   _readPoint = [Location $ EPC "urn:epc:id:sgln:0614141.07346.1234"]
                                                 , _bizLocation = []
                                                 , _srcType = []
                                                 , _destType = []}
                                          , Just DWhere {
                                                   _readPoint = [Location $ EPC "urn:epc:id:sgln:0012345.11111.400"]
                                                 , _bizLocation = [Location $ EPC "urn:epc:id:sgln:0012345.11111.0"]
                                                 , _srcType = []
                                                 , _destType = []}]

  describe "parse QuantityElement" $
    it "parses quantity elements" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEvent2.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "quantityElement" cursor
      parseQuantity <$> oeCursors `shouldBe` [Just $ QuantityElement "urn:epc:class:lgtin:4012345.012345.998877" 200 "KGM"]

  describe "parse object DWhat" $
    it "produces a valid DWhat" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEvent2.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "ObjectEvent" cursor
      parseObjectDWhat <$> oeCursors `shouldBe` [Just $ ObjectDWhat Observe [EPC "urn:epc:id:sgtin:0614141.107346.2017", EPC "urn:epc:id:sgtin:0614141.107346.2018"] [QuantityElement "urn:epc:class:lgtin:4012345.012345.998877" 200 "KGM"]]

  describe "parse object event" $
    it "produces a valid object event" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEvent2.xml"
      let cursor = fromDocument doc
      let parsedEvents = parseObjectEvent cursor
      length parsedEvents `shouldBe` 1
      isJust (head parsedEvents) `shouldBe` True

