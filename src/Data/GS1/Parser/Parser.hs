{-# LANGUAGE OverloadedStrings #-}

{- 
  Unless otherwise stated, all `parse` functions take in a top level cursor
  Here, top level cursor means an event level cursor. following is an example,
  <TransformationEvent>
    <stuff> content </stuff>
    ...
  </TransformationEvent>
-}

module Data.GS1.Parser.Parser where
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Either
import qualified Data.Text           as T
import           Data.Time.LocalTime
import           Data.UUID           hiding (null)
import           Data.XML.Types      hiding (Event)
import           Data.Time

import           Text.Read
import           Text.XML.Cursor

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Except     (MonadError)
import           Control.Monad.Error.Lens

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
parseSingleElemE _ []    = Left TagNotFound

-- |Only the first occurance of EventTime for each Event will be recognised
parseTimeXML :: [T.Text] -> Either ParseFailure EPCISTime
parseTimeXML = parseSingleElemE parseStr2Time

-- |Only the first occurrance of EventTime for each Event will be recognised
parseTimeZoneXML :: [T.Text] -> Either ParseFailure TimeZone
parseTimeZoneXML = parseSingleElemE parseStr2TimeZone

-- |parse the string and obtain TimeZone,
parseStr2TimeZone :: String -> Either ParseFailure TimeZone
parseStr2TimeZone s =
    case parsedStr of
      Just t -> pure t
      Nothing -> Left TimeZoneError
      where
        parsedStr =
            parseTimeM True defaultTimeLocale "%z" s :: Maybe TimeZone

-- TODO IMPORTANT = an error should be returned if no TimeZone
-- use init to remove the 'Z' at the end...
-- TODO = should consider if empty as well
-- example format: 2005-04-03T20:33:31.116-06:00
-- |parse the string to UTC time,
-- the time zone information will be merged into the time
parseStr2Time :: String -> Either ParseFailure EPCISTime
parseStr2Time s =
    case parsedStr of
      Just et -> pure et
      Nothing ->
        case parsedStrZ of
          Just et' -> pure et'
          Nothing  -> Left TimeZoneError
      where
        parsedStr =
            parseTimeM True defaultTimeLocale "%FT%X%Q%z" s :: Maybe EPCISTime
        parsedStrZ =
            parseTimeM True defaultTimeLocale "%FT%X%QZ" s :: Maybe EPCISTime
            -- if this fails, try a different format string

-- |Parse BizStep by Name
parseBizStep :: Cursor -> Either ParseFailure BizStep
parseBizStep c = parseSingleElemE readURI (c $// element "bizStep" &/ content)

-- |Parse Disposition by Name
parseDisposition :: Cursor -> Either ParseFailure Disposition
parseDisposition c = parseSingleElemE readURI
                      (c $// element "disposition" &/ content)

-- |Parse Action by Name
parseAction :: Cursor -> Either ParseFailure Action
parseAction c = parseSingleElemE mkAction (c $// element "action" &/ content)

-- |The name of the current cursor stays at ObjectEvent
parseDWhen :: Cursor -> Either ParseFailure DWhen
parseDWhen c = do
  let etn = c $/ element "eventTime" &/ content
  let tzn = c $/ element "eventTimeZoneOffset" &/ content
  let et = parseTimeXML etn
  let tz = parseTimeZoneXML tzn
  let rt = either2Maybe $ parseTimeXML (c $/ element "recordTime" &/ content)

  case (et, tz) of
    (Right et', Right tz') -> Right $ DWhen et' rt tz'
    _                      -> Left TimeZoneError

-- checks if the bistep is valid for the disposition
-- true if no disposition is found
checkValidBizDisp :: Either ParseFailure BizStep
                      -> Either ParseFailure Disposition -> Bool
-- checkValidBizDisp _ _ = True                      
checkValidBizDisp (Right b) (Right d) = dispositionValidFor b d
checkValidBizDisp (Left TagNotFound) (Left TagNotFound) = True
checkValidBizDisp _ (Left TagNotFound) = True
checkValidBizDisp (Left _) (Right _) = False

-- @todo check for valid combinations of BizStep and Disp
-- |Parse DWhy
parseDWhy :: Cursor -> Either ParseFailure DWhy
parseDWhy c = do
  let biz = parseBizStep c
  let disp = parseDisposition c
  mkDWhy biz disp
  -- comment the following lines in if disp-bizstep combo matters
  -- if checkValidBizDisp biz disp
  --   then
  --     mkDWhy biz disp
  --   else
  --     Left InvalidDispBizCombination

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
    -- possible runtime exception


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


-- insName --> The name of the cursor under which the instanceLabelEPCs lie
-- clName --> The name of the cursor under which the classLabelEPCs lie
-- eg, insName is "inputEPCList", clName is "outputQuantityList", case sensitive
parseLabelEPCs :: Name -> Name -> Cursor -> [Either ParseFailure LabelEPC]
parseLabelEPCs insName clName c = do
  let instanceCursors = getCursorsByName insName c
  let classCursors = flatten $ getCursorsByName "quantityElement" <$>
                        getCursorsByName clName c
  flatten (parseInstanceLabel <$> instanceCursors) ++
    (parseClassLabel <$> classCursors)


-- returns all the errors that occur in Action and [[ParseFailure]],
returnLeftErrors :: (Either ParseFailure Action, [[ParseFailure]])
                    -> ParseFailure
returnLeftErrors (Left act, errs)  = ChildFailure (act : flatten errs)
returnLeftErrors (Right _, errs) = ChildFailure $ flatten errs

-- |parse and construct ObjectDWhat dimension
parseObjectDWhat :: Cursor -> Either ParseFailure DWhat
parseObjectDWhat c = do
  let act = parseAction c
  let (errs, epcs) = partitionEithers $
        parseLabelEPCs "epcList" "quantityList" c
  case (act, errs) of
    (Right a, []) -> Right $ ObjectDWhat a epcs
    _             -> Left $ returnLeftErrors (act, [errs])

-- |parse and construct AggregationDWhat dimension
parseAggregationDWhat :: Cursor -> Either ParseFailure DWhat
parseAggregationDWhat c = do
  let pid = parseParentID c
  let (errs, epcs) = partitionEithers $
        parseLabelEPCs "childEPCs" "childQuantityList" c
  let act = parseAction c

  case (act, errs) of
    (Right a, []) -> Right $ AggregationDWhat a pid epcs
    _             -> Left $ returnLeftErrors (act, [errs])

parseTransactionDWhat :: Cursor -> Either ParseFailure DWhat
parseTransactionDWhat c = do
  let (bizTErrs, bizT) = partitionEithers $ parseBizTransaction c
  let pid = parseParentID c
  let (epcErrs, epcs) = partitionEithers $
        parseLabelEPCs "epcList" "quantityList" c
  let act = parseAction c

  case (act, bizTErrs, epcErrs) of
    (Right a, [], []) -> Right $ TransactionDWhat a pid bizT epcs
    _                 -> Left  $ returnLeftErrors (act, [bizTErrs, epcErrs])

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
              -> [(Maybe EventID
                  , Either ParseFailure DWhat
                  , Either ParseFailure DWhen
                  , Either ParseFailure DWhy
                  , Either ParseFailure DWhere)]
              -> [Either ParseFailure Event]
parseEventList t = fmap asEvent
  where
    asEvent :: (Maybe EventID
              , Either ParseFailure DWhat
              , Either ParseFailure DWhen
              , Either ParseFailure DWhy
              , Either ParseFailure DWhere) -> Either ParseFailure Event
    asEvent (i, w1, w2, w3, w4) = Event t i <$> w1 <*> w2 <*> w3 <*> w4

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

-- this function takes in the top-most cursor
-- | Find all events (that match the specified EventType)
-- and put them into an event list
parseEventByType :: Cursor -> EventType -> [Either ParseFailure Event]
parseEventByType c et = do
  let tagS = case et of
               ObjectEventT         -> "ObjectEvent"
               AggregationEventT    -> "AggregationEvent"
               TransactionEventT    -> "TransactionEvent"
               TransformationEventT -> "TransformationEvent"

  let eCursors = getCursorsByName tagS c
  let eid = either2Maybe . parseEventID <$> eCursors

  let dwhat = parseDWhat et eCursors
  let dwhen = parseDWhen <$> eCursors
  let dwhy = parseDWhy <$> eCursors
  let dwhere = parseDWhere <$> eCursors
  let zipd = zip5 eid dwhat dwhen dwhy dwhere
  parseEventList et zipd
