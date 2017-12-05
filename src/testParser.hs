{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson.Encode.Pretty
import           Data.GS1.Parser.Parser
import           Data.Maybe
import qualified Data.Text as T
import           Text.XML
import           Text.XML.Cursor

main :: IO()
main = do
  doc <- Text.XML.readFile def "../test/test-xml/ObjectEvent.xml"
  let cursor = fromDocument doc
  let oeCursors = getCursorsByName "ObjectEvent" cursor
  let et = "2005-04-03T20:33:31.116-06:00"
  let et1 = "2005-04-04T20:33:31.116-06:00"
  -- print $ head $ fromJust . parseDWhen <$> oeCursors
  -- print $ length oeCursors

  print $ fromJust . parseDWhen <$> oeCursors
  print $ fromJust . parseDWhy <$> oeCursors
  print $ fromJust . parseDWhere <$> oeCursors
  print $ fromJust . parseObjectDWhat <$> oeCursors
  print $ fromJust . parseAggregationDWhat <$> oeCursors
  print $ fromJust . parseTransactionDWhat <$> oeCursors
