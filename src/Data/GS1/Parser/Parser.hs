-- | module providing parser helper functions

{-
  Unless otherwise stated, all `parse` functions take in a top level cursor
  Here, top level cursor means an event level cursor. following is an example,
  <TransformationEvent>
    <stuff> content </stuff>
    ...
  </TransformationEvent>
-}

module Data.GS1.Parser.Parser where
import           Control.Applicative
import           Control.Arrow       hiding (first, second)
import           Data.Bifunctor      (second)
import           Data.Either
import           Data.List
import qualified Data.Text           as T
import           Data.Time
import           Data.UUID           (fromString)
import           Data.XML.Types      hiding (Event)
import           Text.XML.Cursor

import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhy
import           Data.GS1.EPC
import           Data.GS1.Event
import           Data.GS1.EventId
import           Data.GS1.Utils

-- |Get all the cursors with the given name below the current cursor
getCursorsByName :: Name -> Cursor -> [Cursor]
getCursorsByName n c = c $// element n

parseSingleElem :: T.Text
                -> (T.Text -> Either ParseFailure a)
                -> [T.Text]
                -> Either ParseFailure a
parseSingleElem _tag f [x] = f x
parseSingleElem tag _ []   = Left $ TagNotFound (MissingTag tag)
parseSingleElem tag _ _    = Left $ MultipleTags tag

parseTimeXML :: T.Text -> [T.Text] -> Either ParseFailure EPCISTime
parseTimeXML tag = parseSingleElem tag parseStr2Time

parseTimeZoneXML :: T.Text -> [T.Text] -> Either ParseFailure TimeZone
parseTimeZoneXML tag = parseSingleElem tag parseStr2TimeZone

-- |parse the T.Text and obtain TimeZone,
parseStr2TimeZone :: T.Text -> Either ParseFailure TimeZone
parseStr2TimeZone s =
    case parsedStr of
      Just t  -> pure t
      Nothing -> Left $ TimeZoneError (XMLSnippet s)
      where
        parsedStr =
          parseTimeM True defaultTimeLocale "%z" (T.unpack s) :: Maybe TimeZone

{-
All three should have tests
"%FT%X%Q%z" -> 2017-12-20T04:11:43+00:00
"%FT%X%QZ" -> 017-12-20T04:11:43Z
"%Y%m%dT%H%M%S%QZ" -> 20171220T041143Z
-}
isoFormats :: [String]
isoFormats = [
    "%FT%X%Q%z",
    "%FT%X%QZ",
    "%Y%m%dT%H%M%S%QZ"
  ]


getFirstJustTz :: T.Text -> [Maybe a] -> Either ParseFailure a
getFirstJustTz xSnippet []             = Left $ TimeZoneError (XMLSnippet xSnippet)
getFirstJustTz _xmlSnippet (Just x : _)   = Right x
getFirstJustTz xmlSnippet (Nothing : xs) = getFirstJustTz xmlSnippet xs

-- example format: 2005-04-03T20:33:31.116-06:00
-- |parse the string to UTC time,
-- the time zone information will be merged into the time
-- tries the different ISO8601 formats and gets the first one that parses
parseStr2Time :: T.Text -> Either ParseFailure EPCISTime
parseStr2Time s = getFirstJustTz s $
    fmap (\i -> EPCISTime <$> parseTimeM True defaultTimeLocale i (T.unpack s) :: Maybe EPCISTime)
      isoFormats

-- |Parse BizStep by Name
parseBizStep :: Cursor -> Either ParseFailure BizStep
parseBizStep c = parseSingleElem tag readURI (c $// element tagName &/ content)
  where
    tag = "bizStep"
    tagName = "bizStep"

-- |Parse Disposition by Name
parseDisposition :: Cursor -> Either ParseFailure Disposition
parseDisposition c = parseSingleElem tag readURI
                      (c $// element tagName &/ content)
  where
    tag = "disposition"
    tagName = "disposition"

-- |Parse Action by Name
parseAction :: Cursor -> Either ParseFailure Action
parseAction c = parseSingleElem tag mkAction (c $// element tagName &/ content)
  where
    tag = "action"
    tagName = "action"

-- |Requires Event Level cursor
parseDWhen :: Cursor -> Either ParseFailure DWhen
parseDWhen c = do
  let etn = c $/ element "eventTime" &/ content
  let et = parseTimeXML "eventTime" etn
  let tzn = c $/ element "eventTimeZoneOffset" &/ content
  let tz = parseTimeZoneXML "eventTimeZoneOffset" tzn
  let rt = either2Maybe $ parseTimeXML "recordTime" (c $/ element "recordTime" &/ content)

  case (et, tz) of
    (Right et', Right tz') -> Right $ DWhen et' rt tz'
    _                      -> Left $ TimeZoneError (XMLSnippet "")

-- checks if the bistep is valid for the disposition
-- true if no disposition is found
checkValidBizDisp :: Either ParseFailure BizStep
                  -> Either ParseFailure Disposition
                  -> Bool
checkValidBizDisp (Right b) (Right d) = dispositionValidFor b d
checkValidBizDisp (Left (TagNotFound _)) (Left (TagNotFound _)) = True
checkValidBizDisp _ (Left (TagNotFound _)) = True
checkValidBizDisp (Left _) (Right _) = False
checkValidBizDisp _        _         = False

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
{-
c --> Can be a top level / event-level cursor
list --> The Name of the cursor under which the list of epcs lie
         e.g -> sourceList, destinationList
el --> The Name of cursor which has the epc string as its content
         e.g -> source, destination
         The content of this forms the LocationEPC
attr --> The name of the attribute to look into for epc content, e.g, "type"
         The content found with this form the SourceDestTypes
 -}
parseSourceDestLocation :: Cursor -> Name -> Name -> Name ->
                            [Either ParseFailure SrcDestLocation]
parseSourceDestLocation c listTag el attr = do
  -- scope for optimisation: factor out (c $// element listTag &/ element el)
  let locations =
        T.strip <$> (c $// element listTag &/ element el &/ content)
  let srcDestTypes =
        T.strip <$> concat
          (c $// element listTag &/ element el &| attribute attr)
  uncurry (liftA2 (curry SrcDestLocation)) . (readURI *** readURI) <$> zip srcDestTypes locations


  -- TODO: This looks like a great place to use the Validation type
parseDWhere :: Cursor -> Either ParseFailure DWhere
parseDWhere c = do
  let (rpsErrs, rps) = partitionEithers $ readURI <$>
          (c $/ element "readPoint"   &/ element "id" &/ content)
  let (blsErrs, bls) = partitionEithers $ readURI <$>
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
    [[q], []] -> Just $ ItemCount (read (T.unpack q) :: Integer)
    [[q], [u]] -> Just $ MeasuredQuantity (Amount $ read (T.unpack q) :: Amount) (Uom u)
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
  readLabelEPC Nothing <$> (c $/ element "epc" &/ content)

{-
This function expects a cursor that resembles something like:
<quantityElement>
  <epcClass>urn:epc:class:lgtin:4012345.011111.4444</epcClass>
  <quantity>10</quantity>
  <uom>KGM</uom>
</quantityElement>
-}
parseClassLabel :: Cursor -> Either ParseFailure LabelEPC
parseClassLabel c =
  case c $/ element "epcClass" &/ content of
    (labelStr:_) -> readLabelEPC mQt labelStr
    []           -> Left $ TagNotFound (MissingTag "epcClass")
  where
    mQt = parseQuantity c

-- |parse group of text to obtain ParentId
-- takes in one event cursor, looks for a cursor that resembles
{-
<parentId>urn:epc:id:sscc:0614141.1234567890</parentId>
-}
-- and returns the equivalent instanceLabel data-type
parseParentLabel :: Cursor -> Maybe ParentLabel
parseParentLabel c =
  case c $/ element "parentID" &/ content of
    (p:_) -> (either2Maybe . readURI) p
    _     -> Nothing


-- insName --> The name of the cursor under which the instanceLabelEPCs lie
-- clName --> The name of the cursor under which the classLabelEPCs lie
-- eg, insName is "inputEPCList", clName is "outputQuantityList", case sensitive
parseLabelEPCs :: Name -> Name -> Cursor -> [Either ParseFailure LabelEPC]
parseLabelEPCs insName clName c = do
  let instanceCursors = getCursorsByName insName c
  let classCursors = concat $ getCursorsByName "quantityElement" <$>
                        getCursorsByName clName c
  concat (parseInstanceLabel <$> instanceCursors) ++
    (parseClassLabel <$> classCursors)


-- returns all the errors that occur in Action and [[ParseFailure]],
returnLeftErrors :: (Either ParseFailure Action, [[ParseFailure]])
                    -> ParseFailure
returnLeftErrors (Left act, errs) = ChildFailure (act : concat errs)
returnLeftErrors (Right _, errs)  = ChildFailure $ concat errs

-- |parse and construct ObjectDWhat dimension
parseObjectDWhat :: Cursor -> Either ParseFailure ObjectDWhat
parseObjectDWhat c = do
  let act = parseAction c
  let (errs, epcs) = partitionEithers $
        parseLabelEPCs "epcList" "quantityList" c
  case (act, errs) of
    (Right a, []) -> Right $ ObjectDWhat a epcs
    _             -> Left $ returnLeftErrors (act, [errs])

-- |parse and construct AggregationDWhat dimension
parseAggregationDWhat :: Cursor -> Either ParseFailure AggregationDWhat
parseAggregationDWhat c = do
  let pid = parseParentLabel c
  let (errs, epcs) = partitionEithers $
        parseLabelEPCs "childEPCs" "childQuantityList" c
  let act = parseAction c

  case (act, errs) of
    (Right a, []) -> Right $ AggregationDWhat a pid epcs
    _             -> Left $ returnLeftErrors (act, [errs])

parseTransactionDWhat :: Cursor -> Either ParseFailure TransactionDWhat
parseTransactionDWhat c = do
  let (bizTErrs, bizT) = partitionEithers $ parseBizTransaction c
  let pid = parseParentLabel c
  let (epcErrs, epcs) = partitionEithers $
        parseLabelEPCs "epcList" "quantityList" c
  let act = parseAction c

  case (act, bizTErrs, epcErrs) of
    (Right a, [], []) -> Right $ TransactionDWhat a pid bizT epcs
    _                 -> Left  $ returnLeftErrors (act, [bizTErrs, epcErrs])

parseTransformationId :: Cursor -> Maybe TransformationId
parseTransformationId c = do
  let tId = c $/ element "transformationID" &/ content
  case tId of
    [t] -> fmap TransformationId . fromString . T.unpack $ t
    _   -> Nothing

-- EPCIS-Standard-1.2-r-2016-09-29.pdf Page 102
parseTransformationDWhat :: Cursor -> Either ParseFailure TransformationDWhat
parseTransformationDWhat c = do
  -- get transformaiton id
  let tId = parseTransformationId c
  let (inputErrs, inputEpcs) = fmap (fmap InputEPC) . partitionEithers $
        parseLabelEPCs "inputEPCList" "inputQuantityList" c
  let (outputErrs, outputEpcs) = fmap (fmap OutputEPC ) . partitionEithers $
        parseLabelEPCs "outputEPCList" "outputQuantityList" c
  case (inputErrs, outputErrs) of
    ([], []) -> Right $ TransformationDWhat tId inputEpcs outputEpcs
    _        -> Left $ ChildFailure $ inputErrs ++ outputErrs

parseBizTransactionHelp :: (T.Text, T.Text)
                        -> Either ParseFailure BizTransaction
parseBizTransactionHelp (a, b) = do
  let tId   = BizTransactionId $ T.strip a
  let tType = readURI $ T.strip b
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
              -> [(Maybe EventId
                  , Either ParseFailure DWhat
                  , Either ParseFailure DWhen
                  , Either ParseFailure DWhy
                  , Either ParseFailure DWhere)]
              -> [Either ParseFailure Event]
parseEventList t = fmap asEvent
  where
    asEvent :: (Maybe EventId
              , Either ParseFailure DWhat
              , Either ParseFailure DWhen
              , Either ParseFailure DWhy
              , Either ParseFailure DWhere) -> Either ParseFailure Event
    asEvent (i, w1, w2, w3, w4) = Event t i <$> w1 <*> w2 <*> w3 <*> w4

parseEventId :: Cursor -> Either ParseFailure EventId
parseEventId c = do
  let eid = c $/ element "eventID" &/ content
  parseSingleElem "eventID" parseEventId' eid
    where
      parseEventId' eid' = case fromString (T.unpack eid') of
                            Nothing -> Left $ InvalidEventId (EventIdStr eid')
                            Just u  -> Right $ EventId u

parseDWhat :: EventType -> [Cursor] -> [Either ParseFailure DWhat]
parseDWhat ObjectEventT eCursors =
    (second ObjWhat) . parseObjectDWhat <$> eCursors
parseDWhat AggregationEventT eCursors =
    (second AggWhat) . parseAggregationDWhat <$> eCursors
parseDWhat TransactionEventT eCursors =
    (second TransactWhat) . parseTransactionDWhat <$> eCursors
parseDWhat TransformationEventT eCursors =
    (second TransformWhat) . parseTransformationDWhat <$> eCursors

-- this function takes in the top-most cursor
-- | Find all events (that match the specified EventType)
-- and put them into an event list
parseEventByType :: Cursor -> EventType -> [Either ParseFailure Event]
parseEventByType c et =
  let tagS = stringify et
      eCursors = getCursorsByName tagS c
      eid = either2Maybe . parseEventId <$> eCursors
      dwhat = parseDWhat et eCursors
      dwhen = parseDWhen <$> eCursors
      dwhy = parseDWhy <$> eCursors
      dwhere = parseDWhere <$> eCursors
      zipd = zip5 eid dwhat dwhen dwhy dwhere
  in
      parseEventList et zipd
