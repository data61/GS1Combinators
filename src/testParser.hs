{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson.Encode.Pretty
import           Data.GS1.Parser.Parser
import           Data.Maybe
import           Data.Either
import qualified Data.Text as T
import           Text.XML
import           Text.XML.Cursor
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TLE

extractStuff :: Cursor -> Name -> [T.Text]
-- extractStuff (NodeElement a) s = c $/ element s &/ content
extractStuff c s = error "not implemented yet"

-- extractStuff c s = c $/ axis &/ content

extractList :: [Cursor] -> Name -> [[T.Text]]
extractList [] _ = []
extractList (c:cs) s = extractStuff c s : extractList cs s

myName :: Name
myName = "destination"

-- parsePerson :: Cursor -> [Person]
parseDest c = c $/ element myName &/ content


main :: IO()
main = do
  doc <- Text.XML.readFile def "../test/test-xml/ObjectEvent2.xml"
  let cursor = fromDocument doc
  let cursorList = getCursorsByName myName cursor
  let extractedList = extractList cursorList myName
  -- let src = cursor $/ element "source" &/ content
  -- print $ parseDWhen <$> oeCursors


  -- look at src/.../Parser.hs:272. they extracted attributes there
  print $ cursor $// element "destinationList" >=> parseDest
  -- print extractedList


  -- print $ fromJust . parseDWhy <$> oeCursors
  -- print $ fromJust . parseDWhere <$> oeCursors
  -- print $ fromJust . parseObjectDWhat <$> oeCursors
  -- print $ fromJust . parseAggregationDWhat <$> oeCursors
  -- TL.putStrLn . TLE.decodeUtf8 $ encodePretty $ fromJust . parseDWhen <$> oeCursors
