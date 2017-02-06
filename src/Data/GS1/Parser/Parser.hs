{-# LANGUAGE OverloadedStrings #-}

module Data.GS1.Parser.Parser where

import           Control.Lens         hiding (element)
import           Data.GS1.BizStep
import           Data.GS1.Disposition
import           Data.GS1.DWhat
import           Data.GS1.DWhy
import           Data.GS1.EPC
import           Data.GS1.EPCISTime
import           Data.GS1.Event
import           Data.GS1.EventID
import           Data.GS1.Location
import           Data.GS1.Object
import           Data.GS1.Utils
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Text            as T
import           Data.Time.LocalTime
import           Data.UUID
import           Data.UUID.V1
import           Data.XML.Types       hiding (Event)
import           Text.Read
import           Text.XML.Cursor

-- |Get all the cursors with the given name below the current cursor
getCursorsByName :: Name -> Cursor -> [Cursor]
getCursorsByName n c = c $// element n

-- |Only the first occurance of EventTime for each Event will be recognised
parseTimeXML :: [T.Text] -> Maybe EPCISTime
parseTimeXML t = case t of
                   (x:_) -> let pt = parseStr2Time (T.unpack x) :: Either EPCISTimeError EPCISTime in
                                 case pt of
                                   Left _  -> Nothing
                                   Right a -> Just a
                   _      -> Nothing

-- |Only the first occurrance of EventTime for each Event will be recognised
parseTimeZoneXML :: [T.Text] -> Maybe TimeZone
parseTimeZoneXML t = case t of
                       (x:_) -> let ptz = parseStr2TimeZone (T.unpack x) :: Either EPCISTimeError TimeZone in
                                     case ptz of
                                       Left _  -> Nothing
                                       Right a -> Just a
                       _      -> Nothing

-- |Parse TimeZone from eventTimeZoneOffset
-- Only the first occured TimeZone will be considered
parseTimeZoneXML' :: [T.Text] -> Maybe TimeZone
parseTimeZoneXML' [] = Nothing
parseTimeZoneXML' (t:_) = let l = splitOn ":" (T.unpack t) in
                              case l of
                                (x:_) -> let rx = readMaybe x :: Maybe Int in
                                             case rx of
                                               Just t'  -> Just $ hoursToTimeZone t'
                                               Nothing  -> Nothing
                                _      -> Nothing

-- |The name of the current cursor stays at ObjectEvent
parseDWhen :: Cursor -> Maybe DWhen
parseDWhen c = do
  let etn = c $/ element "eventTime" &/ content
  let rtn = c $/ element "recordTime" &/ content
  let tzn = c $/ element "eventTimeZoneOffset" &/ content
  let et = parseTimeXML etn
  let rt = parseTimeXML rtn
  let tz = if isNothing $ parseTimeZoneXML' tzn then parseTimeZoneXML' tzn else parseTimeZoneXML etn
  case et of
    Just et' -> Just (DWhen et' rt (fromJust tz))
    _        -> Nothing

parseAction :: [T.Text] -> Maybe Action
parseAction t = case t of
                  (x:_) -> mkAction . T.unpack $ x
                  _     -> Nothing

parseEPCList :: [T.Text] -> [EPC]
parseEPCList ts = fromJust <$> (mkEPC "EPC" . T.unpack <$> ts)

parseBizStep :: [T.Text] -> Maybe BizStep
parseBizStep ts = case ts of
                    (x:_) -> mkBizStep . T.unpack $ x
                    _     -> Nothing

parseDisposition :: [T.Text] -> Maybe Disposition
parseDisposition ts = case ts of
                        (x:_) -> mkDisposition . T.unpack $ x
                        _     -> Nothing

parseDWhy :: Cursor -> Maybe DWhy
parseDWhy c = do
  let biz = c $/ element "bizStep" &/ content
  let disp = c $/ element "disposition" &/ content
  let pbiz = parseBizStep biz
  let pdisp = parseDisposition disp
  mkDWhy pbiz pdisp

parseQuantity :: Cursor -> Maybe QuantityElement
parseQuantity c = do
  let ec = c $/ element "epcClass" &/ content
  let qt = c $/ element "quantity" &/ content
  let uom = c $/ element "uom" &/ content
  case ec of
    []    -> Nothing
    (e:_) -> case qt of
               []    -> Nothing
               (q:_) -> case uom of
                          []    -> Nothing
                          (u:_) -> let [e', q', u'] = T.unpack <$> [e, q, u] in
                                   Just $ QuantityElement e' (read q' :: Integer) u'

-- | Recusrively construct ObjectDWhat dimension
parseObjectDWhat :: Cursor -> Maybe DWhat
parseObjectDWhat c = do
  -- find action right below ObjectEvent tag
  let act = c $/ element "action" &/ content
  -- find all epcs below epcList tag
  let epc = c $/ element "epcList" &/ element "epc" &/ content
  -- find all quantityElement, regardless parent tag
  let qt  = getCursorsByName "quantityElement" c

  -- parsed action
  let pact = parseAction act
  -- parsed epc
  let pepc = parseEPCList epc
  -- parsed quantity
  let pq = fromJust <$> filter isJust (parseQuantity <$> qt)

  case pact of
    Nothing -> Nothing
    Just p  -> Just $ ObjectDWhat p pepc pq


-- |TODO: due to lack of data, source destination type might not be implemented for now
-- there could be multiple readpoints and bizlocations
-- and there could be no srcDest Type involved
-- the sgln could be irregular
-- |TODO: there must be some more modification on it
parseDWhere :: Cursor -> Maybe DWhere
parseDWhere c = do
  let rp = c $/ element "readPoint"   &/ element "id" &/ content
  let bl = c $/ element "bizLocation" &/ element "id" &/ content
  let rps = (mkLocation . T.unpack) <$> rp
  let bls = (mkLocation . T.unpack) <$> bl
  Just $ DWhere rps bls [] []

parseEventList' :: EventType -> [(Maybe EventID, Maybe DWhat, Maybe DWhen, Maybe DWhy, Maybe DWhere)] -> [Maybe Event]
parseEventList' et l = case l of
                         []     -> []
                         (x:xs) -> let i  = x^._1
                                       w1 = x^._2
                                       w2 = x^._3
                                       w3 = x^._4
                                       w4 = x^._5 in
                                       if isNothing i  ||
                                          isNothing w1 ||
                                          isNothing w2 ||
                                          isNothing w3 ||
                                          isNothing w4 then
                                          Nothing : parseEventList' et xs      else
                                          mkEvent et (fromJust i) (fromJust w1) (fromJust w2) (fromJust w3) (fromJust w4) : parseEventList' et xs

parseEventID :: Cursor -> Maybe EventID
parseEventID c = do
  let eid = c $/ element "eventID" &/ content
  case eid of
    []    -> Nothing
    (x:_) -> let uuid = fromText x in
                 case uuid of
                   Nothing -> Nothing
                   Just u  -> Just $ EventID u

-- | Find all object events
parseObjectEvent :: Cursor -> [Maybe Event]
parseObjectEvent c = do
  let oeCursors = c $// element "ObjectEvent"
  let eventID = parseEventID <$> oeCursors
  let dwhat = parseObjectDWhat <$> oeCursors
  let dwhen = parseDWhen <$> oeCursors
  let dwhy = parseDWhy <$> oeCursors
  let dwhere = parseDWhere <$> oeCursors
  let zipd = zip5 eventID dwhat dwhen dwhy dwhere
  --fromJust <$> filter isJust (parseEventList' ObjectEventT zipd)
  parseEventList' ObjectEventT zipd

parseEventByType :: Cursor -> EventType -> [Maybe Event]
parseEventByType c et = do
  let tagS = case et of
               ObjectEventT         -> "ObjectEvent"
               AggregationEventT    -> "AggregationEvent"
               QuantityEventT       -> "QuantityEvent"
               TransactionEventT    -> "TransactionEvent"
               TransformationEventT -> "TransformationEvent"
  let eCursors = c $// element tagS
  -- TODO
  let eventID = parseEventID <$> eCursors
  let dwhat = case et of
                ObjectEventT        -> parseObjectDWhat <$> eCursors
  let dwhen = parseDWhen <$> eCursors
  let dwhy = parseDWhy <$> eCursors
  let dwhere = parseDWhere <$> eCursors
  let zipd = zip5 eventID dwhat dwhen dwhy dwhere
  parseEventList' et zipd
