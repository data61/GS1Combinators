{-# LANGUAGE OverloadedStrings #-}

module Data.GS1.Parser.Parser where
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Text           as T
import           Data.Time.LocalTime
import           Data.UUID
import           Data.XML.Types      hiding (Event)
import           Text.Read
import           Text.XML.Cursor

import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhy
import           Data.GS1.EPC
import           Data.GS1.Event
import           Data.GS1.EventID
import           Data.GS1.Object

{-

-- |Get all the cursors with the given name below the current cursor
getCursorsByName :: Name -> Cursor -> [Cursor]
getCursorsByName n c = c $// element n

-- |Given a list of Text for a given element
-- Only return the first one
parseSingleElem' :: (String -> Maybe a) -> [T.Text] -> Maybe a
parseSingleElem' f t = case t of
                         (x:_) -> f . T.unpack $ x
                         _     -> Nothing

-- |Parse a list of Text to a list of type a
parseListElem' :: (String -> Maybe a) -> [T.Text] -> [a]
parseListElem' f t = fromJust <$> (f . T.unpack <$> t)

-- |Only the first occurance of EventTime for each Event will be recognised
parseTimeXML :: [T.Text] -> Maybe EPCISTime
parseTimeXML = parseSingleElem' parseTimeHelper'
                 where parseTimeHelper' x = let pt = parseStr2Time x :: Either EPCISTimeError EPCISTime in
                         case pt of
                           Left _  -> Nothing
                           Right a -> Just a

-- |Only the first occurrance of EventTime for each Event will be recognised
parseTimeZoneXML :: [T.Text] -> Maybe TimeZone
parseTimeZoneXML = parseSingleElem' parseTimeZoneHelper'
                       where parseTimeZoneHelper' x = let ptz = parseStr2TimeZone x :: Either EPCISTimeError TimeZone in
                               case ptz of
                                 Left _  -> Nothing
                                 Right a -> Just a

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
  let tzn = c $/ element "eventTimeZoneOffset" &/ content
  let et = parseTimeXML etn
  let tz = if isNothing $ parseTimeZoneXML' tzn then parseTimeZoneXML' tzn else parseTimeZoneXML etn

  let rt = parseTimeXML (c $/ element "recordTime" &/ content)
  case et of
    Just et' -> Just (DWhen et' rt (fromJust tz))
    _        -> Nothing

-- |Parse DWhy
parseDWhy :: Cursor -> Maybe DWhy
parseDWhy c = do
  let biz = parseBizStep (c $/ element "bizStep" &/ content)
  let disp = parseDisposition (c $/ element "disposition" &/ content)
  mkDWhy biz disp

-- |TODO: due to lack of data, source destination type might not be implemented for now
-- there could be multiple readpoints and bizlocations
-- and there could be no srcDest Type involved
-- the sgln could be irregular
-- |TODO: there must be some more modification on it
parseDWhere :: Cursor -> Maybe DWhere
parseDWhere c = do
  let rps = (mkLocation . T.unpack) <$> (c $/ element "readPoint"   &/ element "id" &/ content)
  let bls = (mkLocation . T.unpack) <$> (c $/ element "bizLocation" &/ element "id" &/ content)
  Just $ DWhere rps bls [] []

-- |Parse QuantityElement
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
                          []    -> let [e', q'] = T.unpack <$> [e, q] in
                                       Just $ QuantityElement (EPCClass e') (read q' :: Double) Nothing
                          (u:_) -> let [e', q', u'] = T.unpack <$> [e, q, u] in
                                       Just $ QuantityElement (EPCClass e') (read q' :: Double) (Just u')

--}

-- |Parse a List of EPCs
-- name="epcList" type="epcis:EPCListType"
{- XXX - EPC is no longer a type, but a type class.
parseEPCList :: [T.Text] -> [EPC]
parseEPCList = parseListElem' (mkEPC "EPC")

-- |Alias to parseEPCList
-- name="childEPCs" type="epcis:EPCListType"
parseChildEPCList :: [T.Text] -> [EPC]
parseChildEPCList = parseEPCList
-}
  {-
-- |Parse BizStep by Name
parseBizStep :: [T.Text] -> Maybe BizStep
parseBizStep = parseSingleElem' mkBizStep

-- |Parse Disposition by Name
parseDisposition :: [T.Text] -> Maybe Disposition
parseDisposition = parseSingleElem' mkDisposition

-- |Parse Action by Name
parseAction :: [T.Text] -> Maybe Action
parseAction = parseSingleElem' mkAction

-- |Parse a single EPCClass
parseEPCClass :: [T.Text] -> Maybe EPCClass
parseEPCClass = parseSingleElem' mkEPCClass

-- |Parse a single Maybe Integer
parseQuantityValue :: [T.Text] -> Maybe Integer
parseQuantityValue = parseSingleElem' readMaybeInteger where
                          readMaybeInteger x = readMaybe x :: Maybe Integer

-- |parse group of text to obtain ParentID
parseParentID :: [T.Text] -> Maybe ParentID
parseParentID = parseSingleElem' Just

-- |parse and construct ObjectDWhat dimension
parseObjectDWhat :: Cursor -> Maybe DWhat
parseObjectDWhat c = do
  -- find action right below ObjectEvent tag
  let act = parseAction (c $/ element "action" &/ content)
  -- find all epcs below epcList tag
  let epc = parseEPCList (c $/ element "epcList" &/ element "epc" &/ content)
  -- find all quantityElement, regardless parent tag
  let qt  = fromJust <$> filter isJust (parseQuantity <$> getCursorsByName "quantityElement" c)

  case act of
    Nothing -> Nothing
    Just p  -> Just $ ObjectDWhat p epc qt

-- |BizTransactionList element
parseBizTransaction :: Cursor -> [Maybe BizTransaction]
parseBizTransaction c = do
  let texts = c $// element "bizTransaction" &/ content
  let attrs = foldMap id (c $// element "bizTransaction" &| attribute "type")
  let z = zip attrs texts
  parseBizTransactionHelp <$> z
    where parseBizTransactionHelp (a, b) = mkBizTransaction (T.unpack . T.strip $ a) (T.unpack . T.strip $ b)

-- |parse and construct AggregationDWhat dimension
parseAggregationDWhat :: Cursor -> Maybe DWhat
parseAggregationDWhat c = do
  let pid = parseParentID (c $/ element "parentID" &/ content)
  let childEPCs = parseChildEPCList (c $/ element "childEPCs" &/ element "epc" &/ content)
  let act = parseAction (c $/ element "action" &/ content)
  let qt  = fromJust <$> filter isJust (parseQuantity <$> getCursorsByName "quantityElement" c)

  case act of
    Nothing -> Nothing
    Just p  -> Just $ AggregationDWhat p pid childEPCs qt
-}
-- |parse QuantityDWhat dimension
{-
parseQuantityDWhat :: Cursor -> Maybe DWhat
parseQuantityDWhat c = do
  let ec = parseEPCClass (c $/ element "epcClass" &/ content)
  let qt = parseQuantityValue (c $/ element "quantity" &/ content)

  if isNothing ec || isNothing qt
     then Nothing
     else Just $ QuantityDWhat (fromJust ec) (fromJust qt)
-}
  {-
parseTransactionDWhat :: Cursor -> Maybe DWhat
parseTransactionDWhat c = do
  let bizT = fromJust <$> filter isJust (parseBizTransaction c)
  let pid = parseParentID (c $/ element "parentID" &/ content)
  let epcs = parseEPCList (c $/ element "epcList" &/ element "epc" &/ content)
  let act = parseAction (c $/ element "action" &/ content)
  let qt  = fromJust <$> filter isJust (parseQuantity <$> getCursorsByName "quantityElement" c)

  case act of
    Nothing -> Nothing
    Just p  -> Just $ TransactionDWhat p pid bizT epcs qt

-- |parse a list of tuples
-- each tuple consists of Maybe EventID, Maybe DWhat, Maybe DWhen Maybe DWhy and Maybe DWhere, so they might be Nothing
parseEventList' :: EventType -> [(Maybe EventID, Maybe DWhat, Maybe DWhen, Maybe DWhy, Maybe DWhere)] -> [Maybe Event]
parseEventList' et l = case l of
                         []     -> []
                         (x:xs) -> let (i, w1, w2, w3, w4) = x in
                                       if isNothing i  || isNothing w1 || isNothing w2 || isNothing w3 || isNothing w4 then
                                          Nothing : parseEventList' et xs      else
                                          Just (mkEvent et (fromJust i) (fromJust w1) (fromJust w2) (fromJust w3) (fromJust w4)) : parseEventList' et xs

parseEventID :: Cursor -> Maybe EventID
parseEventID c = do
  let eid = c $/ element "eventID" &/ content
  parseSingleElem' parseEventID' eid where
    parseEventID' eid' = let uuid = fromString eid' in
                             case uuid of
                               Nothing -> Nothing
                               Just u  -> Just $ EventID u

-- | Find all events and put them into an event list
parseEventByType :: Cursor -> EventType -> [Maybe Event]
parseEventByType c et = do
  let tagS = case et of
               ObjectEventT         -> "ObjectEvent"
               AggregationEventT    -> "AggregationEvent"
               QuantityEventT       -> "QuantityEvent"
               TransactionEventT    -> "TransactionEvent"
               TransformationEventT -> "TransformationEvent"
  let eCursors = c $// element tagS
  let eid = parseEventID <$> eCursors
  -- TODO Finish the implementation of the other event types
  let dwhat = case et of
                ObjectEventT      -> parseObjectDWhat      <$> eCursors
                AggregationEventT -> parseAggregationDWhat <$> eCursors
                --QuantityEventT    -> parseQuantityDWhat    <$> eCursors
                TransactionEventT -> parseTransactionDWhat <$> eCursors
                --TransformationEventT -> parseTransformationWhat <$> eCursors
                _                 -> const Nothing         <$> eCursors
  let dwhen = parseDWhen <$> eCursors
  let dwhy = parseDWhy <$> eCursors
  let dwhere = parseDWhere <$> eCursors
  let zipd = zip5 eid dwhat dwhen dwhy dwhere
  parseEventList' et zipd
  -}
