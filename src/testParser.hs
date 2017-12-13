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

getTransformationEPCList :: Cursor -> Name -> [T.Text]
getTransformationEPCList c n = c $// element n &/ element "epc" &/ content

parseTransformationWhat :: Cursor -> Either ParseFailure DWhat
parseTransformationWhat c = do
  let (iEpcsErrs, iEpcs) = partitionEithers $ (readURI :: String -> Either ParseFailure InstanceLabelEPC) . T.unpack <$> getTransformationEPCList c "inputEPCList"
  let (oEpcsErrs, oEpcs) = partitionEithers $ (readURI :: String -> Either ParseFailure InstanceLabelEPC) . T.unpack <$> getTransformationEPCList c "outputEPCList"
  let tId = case c $/ element "transformationID" &/ content of
             [] -> Nothing
             t:ts -> Just $ T.unpack t
  case (iEpcsErrs, oEpcsErrs) of
    ([], []) -> Right $ TransformationDWhat tId iEpcs oEpcs
    _        -> Left $ ChildFailure (iEpcsErrs ++ oEpcsErrs)

main :: IO()
main = do
  doc <- Text.XML.readFile def "../test/test-xml/TransformationEvent.xml"
  let cursor = fromDocument doc
  -- let oeCursors = getCursorsByName "TransformationEvent" cursor
  print $ getTransformationEPCList cursor "inputEPCList"
  print $ getTransformationEPCList cursor "outputEPCList"
  print $ Main.parseTransformationWhat cursor
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
