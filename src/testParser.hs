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
import           Data.GS1.EPC
import           Data.GS1.DWhere
import           Control.Applicative

-- these definitions are there for the sole purpose of using these strings as Names in GHCi
myName :: Name
myName = "destination"

sourceList :: Name
sourceList = "sourceList"

source :: Name
source = "source"

dest :: Name
dest = "destination"

destList :: Name
destList = "destinationList"

typeAttr :: Name
typeAttr = "type"

flatten :: [[a]] -> [a]
flatten xs = (\z n -> foldr (flip (foldr z)) n xs) (:) []

parseSourceDestLocationT :: Cursor -> Name -> Name -> Name -> [(T.Text, T.Text)]
parseSourceDestLocationT c lst el attr = do
  let location = T.strip <$> (c $// element lst &/ element el &/ content)
  let srcDestType = T.strip <$> flatten (c $// element lst &/ element el &| attribute attr)
  zip location srcDestType

main :: IO()
main = do
  doc <- Text.XML.readFile def "../test/test-xml/ObjectEvent2.xml"
  let cursor = fromDocument doc
  -- let cursorList = getCursorsByName myName cursor
  -- let extractedList = extractList cursorList myName
  -- let src = cursor $/ element "source" &/ content
  -- print $ parseDWhen <$> oeCursors


  -- look at src/.../Parser.hs:272. they extracted attributes there
  -- print $ cursor $// element "destinationList" >=> parseDest
  -- print $ cursor $// element sourceList &/ element source &/ content -- location
  -- print $ flatten $ cursor $// element sourceList &/ element source &| attribute typeAttr 
  -- print $ cursor $// element "source" &| attribute "type"
  print $ parseSourceDestLocationT cursor sourceList source typeAttr
  print $ parseSourceDestLocation cursor sourceList source typeAttr

  print $ parseSourceDestLocationT cursor destList dest typeAttr
  print $ parseSourceDestLocation cursor destList dest typeAttr
  
  -- print $ cursor $// element destList &/ element dest &/ content -- location
  -- print $ flatten $ cursor $// element destList &/ element dest &| attribute typeAttr 

  -- print extractedList

  -- print $ fromJust . parseDWhy <$> oeCursors
  -- print $ fromJust . parseDWhere <$> oeCursors
  -- print $ fromJust . parseObjectDWhat <$> oeCursors
  -- print $ fromJust . parseAggregationDWhat <$> oeCursors
  -- TL.putStrLn . TLE.decodeUtf8 $ encodePretty $ fromJust . parseDWhen <$> oeCursors
