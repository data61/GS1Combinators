{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson.Encode.Pretty
import           Data.GS1.Parser.Parser
import           Data.Maybe
import           Data.Either
import           Data.Either.Combinators
import qualified Data.Text as T
import           Text.XML
import           Text.XML.Cursor
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.GS1.EPC
import           Data.GS1.DWhere
import           Data.GS1.DWhat
import           Data.GS1.Event

import           Control.Applicative


flatten :: [[a]] -> [a]
flatten xs = (\z n -> foldr (flip (foldr z)) n xs) (:) []

parseSourceDestLocationT :: Cursor -> Name -> Name -> Name -> [(T.Text, T.Text)]
parseSourceDestLocationT c lst el attr = do
  let location = T.strip <$> (c $// element lst &/ element el &/ content)
  let srcDestType = T.strip <$> flatten (c $// element lst &/ element el &| attribute attr)
  zip location srcDestType

-- parseTransformationWhat :: Cursor -> Either ParseFailure DWhat
-- parseTransformationWhat c = do
--   let (iEpcsErrs, iEpcs) = partitionEithers $
--           readLabelEPC <$> 
--           (T.unpack <$> getTransformationEPCList c "inputEPCList") Nothing
--   let (oEpcsErrs, oEpcs) = 
          -- partitionEithers $ (readURI :: String -> Either ParseFailure InstanceLabelEPC) . 
          -- T.unpack <$> getTransformationEPCList c "outputEPCList"
--   let tId = case c $/ element "transformationID" &/ content of
--              [] -> Nothing
--              t:ts -> Just $ T.unpack t
--   case (iEpcsErrs, oEpcsErrs) of
--     ([], []) -> Right $ TransformationDWhat tId iEpcs oEpcs
--     _        -> Left $ ChildFailure (iEpcsErrs ++ oEpcsErrs)

main :: IO()
main = do
  -- doc <- Text.XML.readFile def "../test/test-xml/TransactionEvent.xml"
  doc <- Text.XML.readFile def "../test/test-xml/TransformationEvent.xml"
  
  let mainCursor = fromDocument doc
  let tCursors = getCursorsByName "TransformationEvent" mainCursor
  let classLabelCursors = flatten $ getCursorsByName "inputQuantityList" <$> tCursors
  let quantityElems = flatten $ getCursorsByName "quantityElement" <$> classLabelCursors

  print $ head $ parseTransformationWhat <$> tCursors
  TL.putStrLn . TLE.decodeUtf8 $
    encodePretty $ fromRight' $ head $ parseTransformationWhat <$> tCursors



  -- print $ length $ getCursorsByName "quantityElement" cursor
  -- let tCursor = head $ getCursorsByName "TransactionEvent" cursor
  -- let texts = tCursor $// element "bizTransaction" &/ content
  -- let attrs = foldMap id (tCursor $// element "bizTransaction" &| attribute "type")
  -- print texts
  -- print attrs
  -- print $ parseEventByType cursor TransactionEventT



  -- print $ fromRight' . parseDWhen <$> oeCursors
  -- print $ fromRight' . parseDWhy <$> oeCursors
  -- print $ fromRight' . parseDWhere <$> oeCursors
  -- print $ fromRight' . parseObjectDWhat <$> oeCursors
  -- print $ fromRight' . parseAggregationDWhat <$> oeCursors

--   print $ parseSourceDestLocationT cursor sourceList source typeAttr
--   print $ partitionEithers $ parseSourceDestLocation cursor sourceList source typeAttr

--   print $ parseSourceDestLocationT cursor destList dest typeAttr
--   print $ parseSourceDestLocation cursor destList dest typeAttr

--   print $ blahfoo <$> oeCursors -- this shows that parseBizTransaction is bugged - mkBizTransactionType might not work with fromJust

-- blahfoo c = do
--   let texts = c $// element "bizTransaction" &/ content
--   let attrs = foldMap id (c $// element "bizTransaction" &| attribute "type")
--   let z = zip attrs texts
--   parseBizTransactionHelp <$> z
--     where
--       parseBizTransactionHelp (a, b) =
--         mkBizTransactionType (T.unpack . T.strip $ b)
