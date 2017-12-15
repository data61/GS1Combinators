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

import           Control.Applicative
import           Control.Arrow

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
parseSingleElemE :: (String -> Either ParseFailure a) -> [T.Text]
                      -> Either ParseFailure a
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
  let tz = if isNothing $ parseTimeZoneXML' tzn
            then parseTimeZoneXML' tzn
            else parseTimeZoneXML etn
  -- ^^^ this statement is potentially buggy. firstly, (parseTimeZoneXML' tzn) is being evaluated twice
  -- secondly, it just returns (parseTimeZoneXML' tzn) if isNothing (parseTimeZoneXML' tzn),
  -- which is equivalent to returning Nothing.
  -- if tz == Nothing, (fromJust tz) would throw a run-time exception
  -- this needs a closer look and more robust error handling

  let rt = parseTimeXML (c $/ element "recordTime" &/ content)
  case et of
    Just et' -> Right $ DWhen et' rt (fromJust tz)
    _        -> Left TimeZoneError

-- TODO = CHECK
-- |Parse DWhy
parseDWhy :: Cursor -> Either ParseFailure DWhy
parseDWhy c = do
  let biz = parseBizStep (c $/ element "bizStep" &/ content)
  let disp = parseDisposition (c $/ element "disposition" &/ content)
  mkDWhy biz disp

-- use rights :: [Either a b] -> [b]
-- or, lookup the monadic instance for Either
-- use do notation
extractLocationEPCList :: T.Text -> Either ParseFailure LocationEPC
extractLocationEPCList = readURI . T.unpack

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

-- test/test-xml/ObjectEvent2.xml can be used to test the parser function
parseSourceDestLocation :: Cursor -> Name -> Name -> Name -> 
                            [Either ParseFailure SrcDestLocation]
parseSourceDestLocation c lst el attr = do
  let locations =
        T.unpack . T.strip <$> (c $// element lst &/ element el &/ content)
  let srcDestTypes =
        T.unpack . T.strip <$> flatten 
          (c $// element lst &/ element el &| attribute attr)
  uncurry (liftA2 (,)) . (readURI *** readURI) <$> zip srcDestTypes locations

parseDWhere :: Cursor -> Either ParseFailure DWhere
parseDWhere c = do
  let (rpsErrs, rps) = partitionEithers $ extractLocationEPCList <$>
          (c $/ element "readPoint"   &/ element "id" &/ content)
  let (blsErrs, bls) = partitionEithers $ extractLocationEPCList <$>
          (c $/ element "bizLocation" &/ element "id" &/ content)
  let (srcTypeErrs, srcTypes) = partitionEithers $
        parseSourceDestLocation c "sourceList" "source" "type"
  let (destTypeErrs, destTypes) = partitionEithers $
        parseSourceDestLocation c "destinationList" "destination" "type"

  case (rpsErrs, blsErrs, srcTypeErrs, destTypeErrs) of
    -- get the sourceDestType and put it in place of the empty lists
    ([], [], [], []) -> Right $ DWhere rps bls srcTypes destTypes
    _                -> Left $ ChildFailure $ 
                          rpsErrs ++ blsErrs ++ srcTypeErrs ++ destTypeErrs

parseQuantity :: Cursor -> Maybe Quantity
parseQuantity c = do
  let qt = c $/ element "quantity" &/ content
  let uom = c $/ element "uom" &/ content

  case [qt, uom] of
    [[], _] -> Nothing
    [[q], []] -> let q' = T.unpack q in
        Just $ ItemCount (read q' :: Integer)
    [[q], [u]] -> let [q', u'] = T.unpack <$> [q, u] in
        Just $ MeasuredQuantity (read q' :: Amount) u'
    _       -> Nothing

{-
The cursor level should be:
<inputEPCList>
  <epc>urn:epc:id:sgtin:4012345.011122.25</epc>
  <epc>urn:epc:id:sgtin:4000001.065432.99886655</epc>
</inputEPCList>
-}
parseInstanceLabel :: Cursor -> [Either ParseFailure LabelEPC]
parseInstanceLabel c =
  readLabelEPC Nothing . T.unpack <$> (c $/ element "epc" &/ content)

{-
This function expects a cursor that resembles something like:
<quantityElement>
  <epcClass>urn:epc:class:lgtin:4012345.011111.4444</epcClass>
  <quantity>10</quantity>
  <uom>KGM</uom>
</quantityElement>
-}
parseClassLabel :: Cursor -> Either ParseFailure LabelEPC
parseClassLabel c = readLabelEPC mQt labelStr
  where
    mQt = parseQuantity c
    [labelStr] = T.unpack <$> (c $/ element "epcClass" &/ content)

-- |Parse BizStep by Name
parseBizStep :: [T.Text] -> Either ParseFailure BizStep
parseBizStep = parseSingleElemE readURI

-- |Parse Disposition by Name
parseDisposition :: [T.Text] -> Either ParseFailure Disposition
parseDisposition = parseSingleElemE readURI

-- |Parse Action by Name ---> perhaps deprecated? -@sa
parseAction :: [T.Text] -> Either ParseFailure Action
parseAction = parseSingleElemE mkAction

-- |Parse a single Maybe Integer
-- readMaybe x :: Maybe Integer
parseQuantityValue :: [T.Text] -> Maybe Integer
parseQuantityValue = parseSingleElemM readMaybe


-- |parse group of text to obtain ParentID
-- takes in one event cursor, looks for a cursor that resembles
{-
<parentID>urn:epc:id:sscc:0614141.1234567890</parentID>
-}
-- and returns the equivalent instanceLabel data-type
parseParentID :: Cursor -> Maybe ParentID
parseParentID c =
  case c $/ element "parentID" &/ content of
    (p:_) -> (either2Maybe . readURI . T.unpack) p
    _   -> Nothing

-- |Parse a List of EPCs
-- name="epcList" type="epcis:EPCListType"
-- whoever calls this function should make sure
-- length of the two arguments are same
-- pad the [Maybe Quantity] with Nothings
parseEPCList :: [T.Text] -> [Maybe Quantity] -> [Either ParseFailure LabelEPC]
parseEPCList [] _ = []
parseEPCList (t:ts) [] = readLabelEPC Nothing (T.unpack t) :
                            parseEPCList ts [Nothing]
parseEPCList (t:ts) (q:qs) = readLabelEPC q (T.unpack t) : parseEPCList ts qs

-- |Alias to parseEPCList
-- name="childEPCs" type="epcis:EPCListType"
parseChildEPCList :: [T.Text] -> [Maybe Quantity] -> 
                     [Either ParseFailure LabelEPC]
parseChildEPCList = parseEPCList

-- returns all the errors that occur in Action and [[ParseFailure]],
returnLeftErrors :: (Either ParseFailure Action, [[ParseFailure]])
                    -> ParseFailure
returnLeftErrors (Left act, errs)  = ChildFailure (act : flatten errs)
returnLeftErrors (Right _, errs) = ChildFailure $ flatten errs

-- |parse and construct ObjectDWhat dimension
-- potentially buggy
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
  let pid = parseParentID c
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
  let pid = parseParentID c
  -- this is potentially buggy. quantity should not be parsed blindly like this
  -- it just literally goes through any arbitrary instance of quantity
  -- it does not put the quantity in the approriate LabelEPC
  let qt = parseQuantity <$> getCursorsByName "quantityElement" c
  let (epcErrs, epcs) = partitionEithers $
        parseEPCList (c $/ element "epcList" &/ element "epc" &/ content) qt
  let act = parseAction (c $/ element "action" &/ content)

  case (act, bizTErrs, epcErrs) of
    (Right a, [], []) -> Right $ TransactionDWhat a pid bizT epcs
    _                 -> Left  $ returnLeftErrors (act, [bizTErrs, epcErrs])

-- insName --> The name of the cursor under which the instanceLabelEPCs lie
-- clName --> The name of the cursor under which the classLabelEPCs lie
-- eg, insName is "inputEPCList", clName is "outputQuantityList", case sensitive
parseLabelEPCs :: Name -> Name -> Cursor -> [Either ParseFailure LabelEPC]
parseLabelEPCs insName clName c = do
  let instanceCursors = getCursorsByName insName c
  let classCursors = flatten $ getCursorsByName "quantityElement" <$>
                        getCursorsByName clName c
  flatten (parseInstanceLabel <$> instanceCursors) ++
    (parseClassLabel <$> classCursors) -- error. get the quantityElements first

parseTransformationID :: Cursor -> Maybe TransformationID
parseTransformationID c = do
  let tId = c $/ element "transformationID" &/ content
  case tId of
    [t] -> Just $ T.unpack t
    _   -> Nothing

-- EPCIS-Standard-1.2-r-2016-09-29.pdf Page 102
parseTransformationDWhat :: Cursor -> Either ParseFailure DWhat
parseTransformationDWhat c = do
  -- get transformaiton id
  let tId = parseTransformationID c
  let (inputErrs, inputEpcs) = partitionEithers $
        parseLabelEPCs "inputEPCList" "inputQuantityList" c
  let (outputErrs, outputEpcs) = partitionEithers $
        parseLabelEPCs "outputEPCList" "outputQuantityList" c
  case (inputErrs, outputErrs) of
    ([], []) -> Right $ TransformationDWhat tId inputEpcs outputEpcs
    _        -> Left $ ChildFailure $ inputErrs ++ outputErrs

parseBizTransactionHelp :: (T.Text, T.Text)
                        -> Either ParseFailure BizTransaction
parseBizTransactionHelp (a, b) = do
  let tId   = T.unpack . T.strip $ a
  let tType = readURI (T.unpack . T.strip $ b)
  case tType of
    Right t -> Right $ BizTransaction tId t
    Left  e -> Left e

-- |BizTransactionList element
parseBizTransaction :: Cursor -> [Either ParseFailure BizTransaction]
parseBizTransaction c = do
  let texts = c $// element "bizTransaction" &/ content
  -- looks like "http://transaction.acme.com/po/12345678"
  let attrs = foldMap id (c $// element "bizTransaction" &| attribute "type")
  -- looks like urn:epcglobal:cbv:btt:po
  let z = zip texts attrs
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
parseDWhat TransformationEventT eCursors = parseTransformationDWhat <$> eCursors

-- | Find all events and put them into an event list
parseEventByType :: Cursor -> EventType -> [Either ParseFailure Event]
parseEventByType c et = do
  let tagS = case et of
               ObjectEventT         -> "ObjectEvent"
               AggregationEventT    -> "AggregationEvent"
               TransactionEventT    -> "TransactionEvent"
               TransformationEventT -> "TransformationEvent"

  let eCursors = getCursorsByName tagS c
  let eid = parseEventID <$> eCursors

  let dwhat = parseDWhat et eCursors
  let dwhen = parseDWhen <$> eCursors
  let dwhy = parseDWhy <$> eCursors
  let dwhere = parseDWhere <$> eCursors
  let zipd = zip5 eid dwhat dwhen dwhy dwhere
  parseEventList et zipd
