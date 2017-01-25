{-# LANGUAGE OverloadedStrings #-}

module Tests.Parser where

import           Data.GS1.Event
import           Text.XML
import           Text.XML.Cursor
import           XML.Parser

import           Test.Hspec

testParseDWhen :: Spec
testParseDWhen =
  describe "parse XML to obtain DWhen" $ do
    it "creates DWhen from valid XML" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEvent.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "ObjectEvent" cursor
      --mapM_ print $ parseDWhen <$> oeCursors
      let et = "2005-04-03T20:33:31.116-06:00"
      let et1 = "2005-04-04T20:33:31.116-06:00"
      parseDWhen <$> oeCursors `shouldBe` [mkDWhen' et, mkDWhen et1 ""]

    it "creates Nothing from invalid XML when there is no Event Time" $ do
      doc <- Text.XML.readFile def "test/test-xml/ObjectEventNoEventTime.xml"
      let cursor = fromDocument doc
      let oeCursors = getCursorsByName "ObjectEvent" cursor
      parseDWhen <$> oeCursors `shouldBe` [Nothing]

