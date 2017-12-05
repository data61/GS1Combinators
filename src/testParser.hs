{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson.Encode.Pretty
import           Data.GS1.Parser.Parser
import           Data.Maybe
import qualified Data.Text              as T
import           Text.XML
import           Text.XML.Cursor

parseCustom :: String -> String -> (Cursor -> Maybe a) -> String
parseCustom fileName eventName parseFunc = do
  doc <- Text.XML.readFile def fileName
  let cursor = fromDocument doc
  let eventCursors = getCursorsByName eventName cursor
  encodePretty $ head $ fromJust . parseFunc <$> eventCursors

main :: IO()
main = do
  print $ parseCustom "../test/test-xml/ObjectEvent.xml" "ObjectEvent" parseDWhen
  -- print $ parseCustom "../test/test-xml/ObjectEvent.xml"

  -- print $ length oeCursors
