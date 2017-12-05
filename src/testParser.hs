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
  putStr $ show $ encodePretty $ head $ fromJust . parseDWhen <$> oeCursors
