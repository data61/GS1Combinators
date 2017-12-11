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
import           Control.Monad
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
  -- ^^^ this line is potentially buggy. firstly, (parseTimeZoneXML' tzn) is being evaluated twice
  -- secondly, it just returns (parseTimeZoneXML' tzn) if isNothing (parseTimeZoneXML' tzn),
  -- which is equivalent to returning Nothing.
  -- if tz == Nothing, (fromJust tz) would throw a run-time exception
  -- this needs a closer look and more robust error handling

  let rt = parseTimeXML (c $/ element "recordTime" &/ content)
  case et of
    Just et' -> Right $ DWhen et' rt (fromJust tz)
    _        -> Left TimeZoneError

-- |Parse DWhy
parseDWhy :: Cursor -> Either ParseFailure DWhy
parseDWhy c = do
  let biz = parseBizStep (c $/ element "bizStep" &/ content)
  let disp = parseDisposition (c $/ element "disposition" &/ content)
  mkDWhy biz disp

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
  let (lRps, rRps) = partitionEithers $ extractLocationEPCList <$>
          (c $/ element "readPoint"   &/ element "id" &/ content)
  let (lBls, rBls) = partitionEithers $ extractLocationEPCList <$>
          (c $/ element "bizLocation" &/ element "id" &/ content)

  case (lRps, lBls) of
    ([], []) -> Right $ DWhere rRps rBls [] []
    -- get the sourceDestType and put it in place of the empty lists
    _        -> Left $ ChildFailure $ lRps ++ lBls


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
parseAction :: [T.Text] -> Either ParseFailure Action
parseAction = parseSingleElemE mkAction

-- |Parse a single Maybe Integer
-- readMaybe x :: Maybe Integer
parseQuantityValue :: [T.Text] -> Maybe Integer
parseQuantityValue = parseSingleElemM readMaybe

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
parseEPCList :: [T.Text] -> [Maybe Quantity] -> [Either ParseFailure LabelEPC]
parseEPCList [] _ = []
parseEPCList _ [] = []
parseEPCList (t:ts) (q:qs) =
  readLabelEPC (T.unpack t) q : parseEPCList ts qs

-- |Alias to parseEPCList
-- name="childEPCs" type="epcis:EPCListType"
-- parseChildEPCList :: [T.Text] -> [Maybe Quantity] -> [LabelEPC]
parseChildEPCList = parseEPCList

-- returns all the errors that occur in Action and [[ParseFailure]]
returnLeftErrors :: (Either ParseFailure Action, [[ParseFailure]])
  -> ParseFailure
returnLeftErrors (Left act, errs)  = ChildFailure $ act : flatten errs
returnLeftErrors (Right act, errs) = ChildFailure $ flatten errs

-- |parse and construct ObjectDWhat dimension
parseObjectDWhat :: Cursor -> Either ParseFailure DWhat
parseObjectDWhat c = do
  -- find action right below ObjectEvent tag
  let act = parseAction (c $/ element "action" &/ content)
  -- find all epcs below epcList tag
  let qt  = parseQuantity <$> getCursorsByName "quantityElement" c
  let (errs, epcs) = partitionEithers $
        parseEPCList (c $/ element "epcList" &/ element "epc" &/ content) qt

  case (act, errs) of
    (Right a, []) -> Right $ ObjectDWhat a epcs
    _             -> Left $ returnLeftErrors (act, [errs])

-- |parse and construct AggregationDWhat dimension
parseAggregationDWhat :: Cursor -> Either ParseFailure DWhat
parseAggregationDWhat c = do
  let pid = parseParentID (c $/ element "parentID" &/ content)
  let qt  = parseQuantity <$> getCursorsByName "quantityElement" c
  let (errs, epcs) = partitionEithers $ parseChildEPCList
          (c $/ element "childEPCs" &/ element "epc" &/ content) qt
  let act = parseAction (c $/ element "action" &/ content)

  case (act, errs) of
    (Right a, []) -> Right $ AggregationDWhat a pid epcs
    _             -> Left $ returnLeftErrors (act, [errs])

parseTransactionDWhat :: Cursor -> Either ParseFailure DWhat
parseTransactionDWhat c = do
  let (bizTErrs, bizT) = partitionEithers $ parseBizTransaction c
  let pid = parseParentID (c $/ element "parentID" &/ content)
  let qt = parseQuantity <$> getCursorsByName "quantityElement" c
  let (epcErrs, epcs) = partitionEithers $
        parseEPCList (c $/ element "epcList" &/ element "epc" &/ content) qt
  let act = parseAction (c $/ element "action" &/ content)

  case (act, bizTErrs, epcErrs) of
    (Right a, [], []) -> Right $ TransactionDWhat a pid bizT epcs
    _                 -> Left  $ returnLeftErrors (act, [bizTErrs, epcErrs])

parseTransformationWhat :: Cursor -> Either ParseFailure DWhat
parseTransformationWhat c = error "Not implemented yet"

-- TODO add type signature after running `stack test`
parseBizTransactionHelp (a, b) = do
  let tId   = T.unpack . T.strip $ a
  let tType = mkBizTransactionType (T.unpack . T.strip $ b)
  case tType of
    Right t -> Right $ BizTransaction tId t
    _       -> Left InvalidBizTransaction

-- |BizTransactionList element
parseBizTransaction :: Cursor -> [Either ParseFailure BizTransaction]
parseBizTransaction c = do
  let texts = c $// element "bizTransaction" &/ content
  let attrs = foldMap id (c $// element "bizTransaction" &| attribute "type")
  let z = zip attrs texts
  parseBizTransactionHelp <$> z

parseEventList :: EventType
  -> [(Either ParseFailure EventID
       , Either ParseFailure DWhat
       , Either ParseFailure DWhen
       , Either ParseFailure DWhy
       , Either ParseFailure DWhere)]
  -> [Either ParseFailure Event]
parseEventList t = fmap asEvent
  where
    asEvent :: (Either ParseFailure EventID
      , Either ParseFailure DWhat
      , Either ParseFailure DWhen
      , Either ParseFailure DWhy
      , Either ParseFailure DWhere) -> Either ParseFailure Event
    asEvent (i, w1, w2, w3, w4) = Event t <$>  i <*> w1 <*> w2 <*> w3 <*> w4

parseEventID :: Cursor -> Either ParseFailure EventID
parseEventID c = do
  let eid = c $/ element "eventID" &/ content
  parseSingleElemE parseEventID' eid where
    parseEventID' eid' = case fromString eid' of
                           Nothing -> Left InvalidEvent
                           Just u  -> Right $ EventID u

parseDWhat :: EventType -> [Cursor] -> [Either ParseFailure DWhat]
parseDWhat ObjectEventT eCursors = parseObjectDWhat <$> eCursors
parseDWhat AggregationEventT eCursors = parseAggregationDWhat <$> eCursors
parseDWhat TransactionEventT eCursors = parseTransactionDWhat <$> eCursors
parseDWhat TransformationEventT eCursors = parseTransformationWhat <$> eCursors

-- | Find all events and put them into an event list
parseEventByType :: Cursor -> EventType -> [Either ParseFailure Event]
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
  let dwhat = parseDWhat et eCursors
  let dwhen = parseDWhen <$> eCursors
  let dwhy = parseDWhy <$> eCursors
  let dwhere = parseDWhere <$> eCursors
  let zipd = zip5 eid dwhat dwhen dwhy dwhere
  parseEventList et zipd
