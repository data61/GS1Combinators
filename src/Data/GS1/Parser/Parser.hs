{-# LANGUAGE OverloadedStrings #-}

module Data.GS1.Parser.Parser where
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Either
import qualified Data.Text           as T
import           Data.Time.LocalTime
import           Data.UUID
import           Data.XML.Types      hiding (Event)
import           Text.Read
import           Text.XML.Cursor

import           Data.GS1.Utils
import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhy
import           Data.GS1.EPC
import           Data.GS1.Event
import           Data.GS1.EventID

-- |Get all the cursors with the given name below the current cursor
getCursorsByName :: Name -> Cursor -> [Cursor]
getCursorsByName n c = c $// element n


-- can parseSingleElem be more generalised?
-- |Given a list of Text for a given element
-- Only return the first one
-- parseSingleElemM returns a Maybe
parseSingleElemM :: (String -> Maybe a) -> [T.Text] -> Maybe a
parseSingleElemM f (x:_) = f . T.unpack $ x
parseSingleElemM _ _     = Nothing

-- parseSingleElemE returns an Either
parseSingleElemE :: (String -> Either ParseFailure a) -> [T.Text] -> Either ParseFailure a
parseSingleElemE f (x:_) = f . T.unpack $ x
parseSingleElemE _ _     = Left InvalidFormat


-- |Parse a list of Text to a list of type a
-- deprecated
parseListElem' :: (String -> Maybe a) -> [T.Text] -> [a]
parseListElem' f t = fromJust <$> (f . T.unpack <$> t)

-- |Only the first occurance of EventTime for each Event will be recognised
parseTimeXML :: [T.Text] -> Maybe EPCISTime
parseTimeXML = parseSingleElemM parseTimeHelper'
                where
                  parseTimeHelper' x =
                    let pt = parseStr2Time x :: Either EPCISTimeError EPCISTime in
                        case pt of
                          Left _  -> Nothing
                          Right a -> Just a

-- |Only the first occurrance of EventTime for each Event will be recognised
parseTimeZoneXML :: [T.Text] -> Maybe TimeZone
parseTimeZoneXML = parseSingleElemM parseTimeZoneHelper'
                      where
                        parseTimeZoneHelper' x =
                          let ptz = parseStr2TimeZone x :: Either EPCISTimeError TimeZone in
                              case ptz of
                                Left _  -> Nothing
                                Right a -> Just a

-- |Parse TimeZone from eventTimeZoneOffset
-- Only the first occured TimeZone will be considered
parseTimeZoneXML' :: [T.Text] -> Maybe TimeZone
parseTimeZoneXML' [] = Nothing
parseTimeZoneXML' (t:_) = let l = splitOn ":" (T.unpack t) in
                            case l of
                              (x:_) ->
                                let rx = readMaybe x :: Maybe Int in
                                    case rx of
                                      Just t'  -> Just $ hoursToTimeZone t'
                                      Nothing  -> Nothing
                              _     -> Nothing

-- |The name of the current cursor stays at ObjectEvent
parseDWhen :: Cursor -> Either ParseFailure DWhen
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
parseDWhy :: Cursor -> Either ParseFailure DWhy
parseDWhy c = do
  let biz = parseBizStep (c $/ element "bizStep" &/ content)
  let disp = parseDisposition (c $/ element "disposition" &/ content)
  mkDWhy biz disp

-- should getRightOrError be used here?
-- use rights :: [Either a b] -> [b]
-- or, lookup the monadic instance for Either
-- use do notation
extractLocationEPCList :: T.Text -> Either ParseFailure ReadPointLocation
extractLocationEPCList = readURI . T.unpack


-- |TODO: due to lack of data, source destination type might not be implemented for now
-- there could be multiple readpoints and bizlocations
-- and there could be no srcDest Type involved
-- the sgln could be irregular
-- |TODO: there must be some more modification on it

-- SIDE NOTE:
-- it might be helpful to write a function like,
-- f :: [Either x y] -> Either [x] [y]

{-
(>>=) :: m a -> (a -> m b) -> m b

do
  x <- y
  f x

  is equivalent to
  y >>= \x -> f x

-}
parseDWhere :: Cursor -> Either ParseFailure DWhere
parseDWhere c = do
  -- splitEither :: [Either a b] -> ([a], [b])
  let (lRps, rRps) = partitionEithers $ extractLocationEPCList <$>
          (c $/ element "readPoint"   &/ element "id" &/ content)

  let (lBls, rBls) = partitionEithers $ extractLocationEPCList <$>
          (c $/ element "bizLocation" &/ element "id" &/ content)

  case (lRps, lBls) of
    ([], []) -> Right $ DWhere rRps rBls [] []
    -- why are the last two values always empty lists?
    _        -> Left $ ChildFailure $ lRps ++ lBls

  -- bls <- extractLocationEPCList <$>
  --   (c $/ element "bizLocation" &/ element "id" &/ content)
  -- let rps = extractLocationEPCList <$>
  --         (c $/ element "readPoint"   &/ element "id" &/ content)
  -- let bls = extractLocationEPCList <$>
  --         (c $/ element "bizLocation" &/ element "id" &/ content)
  -- Just $ DWhere rps bls [] [] -- why is this always returning empty lists?
  -- pure $ DWhere rps bls [] [] -- why is this always returning empty lists?
-- ^^ use the pure function here

-- this is potentially buggy. why does it return/parse only the first quantity?
-- look into how Cursor works to figure this out
parseQuantity :: Cursor -> Maybe Quantity
parseQuantity c = do
  let qt = c $/ element "quantity" &/ content
  let uom = c $/ element "uom" &/ content

  case [qt, uom] of
    [[], _] -> Nothing
    [q:_, []] -> let q' = T.unpack q in
        Just $ ItemCount (read q' :: Integer)
    [q:_, u:_] -> let [q', u'] = T.unpack <$> [q, u] in
        Just $ MeasuredQuantity (read q' :: Amount) u'

-- |Parse BizStep by Name
parseBizStep :: [T.Text] -> Either ParseFailure BizStep
parseBizStep = parseSingleElemE readURI

-- |Parse Disposition by Name
parseDisposition :: [T.Text] -> Either ParseFailure Disposition
parseDisposition = parseSingleElemE readURI

-- |Parse Action by Name ---> perhaps deprecated? -@sa
-- Action is not a URI, so I am making parseAction return a Maybe Action
-- as opposed to any of the other parse[a] functions,
-- which returns Either ParseFailure a.
parseAction :: [T.Text] -> Maybe Action
parseAction = parseSingleElemM mkAction

-- |Parse a single Maybe Integer
parseQuantityValue :: [T.Text] -> Maybe Integer
-- parseQuantityValue = parseSingleElemM (readMaybe :: Maybe Integer)
-- why doesn't this work?
parseQuantityValue = parseSingleElemM readMaybeInteger where
                        readMaybeInteger x = readMaybe x :: Maybe Integer

-- |parse group of text to obtain ParentID
-- a sample usage of this function would have been nice
-- previous implementation likely wouldn't work.
-- needs a (text -> URI a) function or something
-- current implementation might be potentially buggy,
-- as I am not sure how this function is supposed to work
parseParentID :: [T.Text] -> Maybe ParentID
parseParentID [] = Nothing
parseParentID (t:ts)
  | isJust returnValue = returnValue
  | otherwise          = parseParentID ts
  where returnValue = mkByName $ T.unpack t

-- |Parse a List of EPCs
-- name="epcList" type="epcis:EPCListType"
-- not a good time to get out of the Either binding
parseEPCList :: [T.Text] -> [Maybe Quantity] -> [Either ParseFailure LabelEPC]
parseEPCList [] _ = []
parseEPCList _ [] = []
parseEPCList (t:ts) (q:qs) =
  readLabelEPC (T.unpack t) q : parseEPCList ts qs

-- |Alias to parseEPCList
-- name="childEPCs" type="epcis:EPCListType"
-- parseChildEPCList :: [T.Text] -> [Maybe Quantity] -> [LabelEPC]
parseChildEPCList = parseEPCList


-- |parse and construct ObjectDWhat dimension
parseObjectDWhat :: Cursor -> Either ParseFailure DWhat
parseObjectDWhat c = do
  -- find action right below ObjectEvent tag
  let act = parseAction (c $/ element "action" &/ content)
  -- find all epcs below epcList tag
  let qt  = parseQuantity <$> getCursorsByName "quantityElement" c
  let epc = parseEPCList (c $/ element "epcList" &/ element "epc" &/ content) qt

  case act of
    Nothing -> Nothing
    Just p  -> Just $ ObjectDWhat p epc


-- |parse and construct AggregationDWhat dimension
parseAggregationDWhat :: Cursor -> Either ParseFailure DWhat
parseAggregationDWhat c = do
  let pid = parseParentID (c $/ element "parentID" &/ content)
  let qt  = parseQuantity <$> getCursorsByName "quantityElement" c
  let childEPCs = parseChildEPCList (c $/ element "childEPCs" &/ element "epc" &/ content) qt
  let act = parseAction (c $/ element "action" &/ content)

  case act of
    Nothing -> Nothing
    Just p  -> Just $ AggregationDWhat p pid childEPCs

parseTransactionDWhat :: Cursor -> Either ParseFailure DWhat
parseTransactionDWhat c = do
  let bizT = fromJust <$> filter isJust $ parseBizTransaction c
  let pid = parseParentID (c $/ element "parentID" &/ content)
  let qt = parseQuantity <$> getCursorsByName "quantityElement" c
  let epcs = parseEPCList (c $/ element "epcList" &/ element "epc" &/ content) qt
  let act = parseAction (c $/ element "action" &/ content)

  case act of
    Nothing -> Nothing
    Just p  -> Just $ TransactionDWhat p pid bizT epcs

parseTransformationWhat :: Cursor -> Either ParseFailure DWhat
parseTransformationWhat c = error "Not implemented yet"
-- parseTransformationWhat c = do
--   let bizT = fromJust <$> filter isJust (parseBizTransaction c)
--   let pid = parseParentID (c $/ element "parentID" &/ content)
--   let qt = parseQuantity <$> getCursorsByName "quantityElement" c
--   let epcs = parseEPCList (c $/ element "epcList" &/ element "epc" &/ content) qt
--   let act = parseAction (c $/ element "action" &/ content)

--   case act of
--     Nothing -> Nothing
--     Just p  -> Just $ TransactionDWhat p pid bizT epcs

-- |BizTransactionList element
parseBizTransaction :: Cursor -> [Maybe BizTransaction]
parseBizTransaction c = do
  let texts = c $// element "bizTransaction" &/ content
  let attrs = foldMap id (c $// element "bizTransaction" &| attribute "type")
  let z = zip attrs texts
  parseBizTransactionHelp <$> z
    where
      parseBizTransactionHelp (a, b) =
        Just $ BizTransaction (T.unpack . T.strip $ a) $
          fromJust $ mkBizTransactionType (T.unpack . T.strip $ b)
          -- potentially buggy, as it never returns Nothing

-- |parse a list of tuples
-- if we encounter a parsefailure, return the Left listOfParseFailures
parseEventList' :: EventType
  -> [(Maybe EventID
  , Either ParseFailure DWhat
  , Either ParseFailure DWhen
  , Either ParseFailure DWhy
  , Either ParseFailure DWhere)]
  -> [Either ParseFailure Event]
-- parseEventList' = error "not implemented yet"
parseEventList' _ [] = []
parseEventList' et (x:xs) = do
  let (i, w1, w2, w3, w4) = x
  if isNothing i  || isNothing w1 || isNothing w2 || isNothing w3 || isNothing w4 then
    Nothing : parseEventList' et xs      else
    Just (Event et (fromJust i) (fromJust w1) (fromJust w2) (fromJust w3) (fromJust w4)) : parseEventList' et xs

parseEventID :: Cursor -> Maybe EventID
parseEventID c = do
  let eid = c $/ element "eventID" &/ content
  parseSingleElemM parseEventID' eid where
    parseEventID' eid' = case fromString eid' of
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
                -- QuantityEventT    -> parseQuantityDWhat    <$> eCursors
                TransactionEventT -> parseTransactionDWhat <$> eCursors
                TransformationEventT -> parseTransformationWhat <$> eCursors
                _                 -> const Nothing         <$> eCursors
  let dwhen = parseDWhen <$> eCursors
  let dwhy = parseDWhy <$> eCursors
  let dwhere = parseDWhere <$> eCursors
  let zipd = zip5 eid dwhat dwhen dwhy dwhere
  parseEventList' et zipd
