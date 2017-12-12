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

extractStuff :: Cursor -> Name -> [T.Text]
extractStuff c s = error "not implemented yet"

extractList :: [Cursor] -> Name -> [[T.Text]]
extractList [] _ = []
extractList (c:cs) s = extractStuff c s : extractList cs s

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

-- parseSourceDestLocation :: Cursor -> Name -> Name -> Name -> [(Either ParseFailure SourceDestType, Either ParseFailure LocationEPC)]
-- parseSourceDestLocation c lst el attr = do
--   let location = T.unpack . T.strip <$> (c $// element lst &/ element el &/ content)
--   let srcDestType = T.unpack . T.strip <$> flatten (c $// element lst &/ element el &| attribute attr)
--   (\(loc, sdType) -> (readURI sdType, readURI loc)) <$> zip location srcDestType

-- uncurry (liftA2 (,))

parseSourceDestLocationT :: Cursor -> Name -> Name -> Name -> [(T.Text, T.Text)]
parseSourceDestLocationT c lst el attr = do
  let location = T.strip <$> (c $// element lst &/ element el &/ content)
  let srcDestType = T.strip <$> flatten (c $// element lst &/ element el &| attribute attr)
  zip location srcDestType


main :: IO()
main = do
  doc <- Text.XML.readFile def "../test/test-xml/ObjectEvent2.xml"
  let cursor = fromDocument doc
  let oeCursors = getCursorsByName "ObjectEvent" cursor
  -- let et = "2005-04-03T20:33:31.116-06:00"
  -- let et1 = "2005-04-04T20:33:31.116-06:00"
  -- print $ head $ fromJust . parseDWhen <$> oeCursors
  -- print $ length oeCursors

  print  oeCursors

  print $ fromJust . parseDWhen <$> oeCursors
  print $ fromJust . parseDWhy <$> oeCursors
  print $ fromJust . parseDWhere <$> oeCursors
  print $ fromJust . parseObjectDWhat <$> oeCursors
  print $ fromJust . parseAggregationDWhat <$> oeCursors

  print $ parseSourceDestLocationT cursor sourceList source typeAttr
  print $ partitionEithers $ parseSourceDestLocation cursor sourceList source typeAttr

  print $ parseSourceDestLocationT cursor destList dest typeAttr
  print $ parseSourceDestLocation cursor destList dest typeAttr

  print $ blahfoo <$> oeCursors -- this shows that parseBizTransaction is bugged - mkBizTransactionType might not work with fromJust
  -- print $ fromJust . parseTransactionDWhat <$> oeCursors

  ------------------------------------------------------
blahfoo c = do
  let texts = c $// element "bizTransaction" &/ content
  let attrs = foldMap id (c $// element "bizTransaction" &| attribute "type")
  let z = zip attrs texts
  parseBizTransactionHelp <$> z
    where
      parseBizTransactionHelp (a, b) =
        mkBizTransactionType (T.unpack . T.strip $ b)
--        Just $ BizTransaction (T.unpack . T.strip $ a) $
--          fromJust $ mkBizTransactionType (T.unpack . T.strip $ b)
  -- print $ fromJust . parseDWhen <$> oeCursors
  
  -- let cursorList = getCursorsByName myName cursor
  -- let extractedList = extractList cursorList myName
  -- let src = cursor $/ element "source" &/ content
  -- print $ parseDWhen <$> oeCursors


  -- look at src/.../Parser.hs:272. they extracted attributes there
  -- print $ cursor $// element "destinationList" >=> parseDest
  -- print $ cursor $// element sourceList &/ element source &/ content -- location
  -- print $ flatten $ cursor $// element sourceList &/ element source &| attribute typeAttr 
  -- print $ cursor $// element "source" &| attribute "type"

  
  -- print $ cursor $// element destList &/ element dest &/ content -- location
  -- print $ flatten $ cursor $// element destList &/ element dest &| attribute typeAttr 

  -- print extractedList

  -- print $ fromJust . parseDWhy <$> oeCursors
  -- print $ fromJust . parseDWhere <$> oeCursors
  -- print $ fromJust . parseObjectDWhat <$> oeCursors
  -- print $ fromJust . parseAggregationDWhat <$> oeCursors
  -- TL.putStrLn . TLE.decodeUtf8 $ encodePretty $ fromJust . parseDWhen <$> oeCursors
