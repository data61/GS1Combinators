{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.GS1.Parser.Parser where
import           Prelude         hiding (readFile, writeFile)
import Data.Time
import Data.Time.Format

import qualified Data.Map        as M
import           Text.XML
import Data.GS1.Event
import Data.GS1.Location
import Data.GS1.SourceDest
import Data.Maybe

--parse IO :: ()
--"/Users/fal05c/work/gs1combinators/src/Data/GS1/Parser/ObjectEvent.xml"
parse :: [Event] -> FilePath -> IO ()
parse eventList filename = do
    -- readFile will throw any parse errors as runtime exceptions
    -- def uses the default settings
    Document prologue root epilogue <- readFile def filename
    -- root is the root element of the document
    --
    --let eventList = getEvents root
    --print eventList
    let eventList = getEventList root
    print "eventList: --------------------"
    print eventList
    print "endEventList: ------------"
    let timeZoneOffset = fromJust $ searchElement "disposition" (head eventList)
    print "timezoneoffset: -------------"
    print timeZoneOffset
    print "------------------"
    let when = makeWhen (head eventList)
    print when
    print "getBizStep"
    let readPoint = getDisposition (head eventList)
    print readPoint
    --let names = getNames (head eventList)
    --print names



getEventList :: Element -> [Node]
getEventList (Element _ _ nodes) = filter isElement events
  where
    (NodeElement (Element _ _ bodyNodes)) = head(filter isElement nodes)
    (NodeElement (Element _ _ events))= head(filter isElement bodyNodes)



isElement :: Node -> Bool
isElement (NodeElement e) = True
isElement _ = False




----------------------------
-- What  -------------------
----------------------------


--TODO

----------------------------
-- Why ---------------------
----------------------------

--getBizStep :: Node -> Maybe BizStep
--returns Just "urn:epcglobal:cbv:bizstep:receiving"
getBizStep node = if (isNothing bizStepNodes || bizNode == [] )
                     then Nothing else Just bizStepStr
  where
    bizStepNodes = searchElement "bizStep" node
    (Element _ _ bizNode) = head $ fromJust $ bizStepNodes
    (NodeContent bizStepStr) = head bizNode
    -- TODO: parse bizStepStr, and return that instead.


--getDisposition :: Node -> Maybe Disposition
--returns Just "urn:epcglobal:cbv:disp:in_progress"
getDisposition node = if (isNothing dispNodes || dispNode == [] )
                     then Nothing else Just dispStr
  where
    dispNodes = searchElement "disposition" node
    (Element _ _ dispNode) = head $ fromJust $ dispNodes
    (NodeContent dispStr) = head dispNode
    -- TODO: parse disposition, and return that instead.


----------------------------
-- When -------------------
-- -------------------------



--makeWhen :: Node -> (Text, Text)
--FIXME: the parsing doesn't work, and the compiler says it's
--deprecated. Work out what to do! :)
makeWhen node  = (timeString, tzString)
  where
    timeElements = fromJust $ searchElement "eventTime" node
    timeZoneElements = fromJust $ searchElement "eventTimeZoneOffset" node
    (Element _ _ tzNode) = head timeZoneElements
    (NodeContent tzString) = head tzNode --eg: "+02:00"
    (Element _ _ timeNode) = head timeElements
    (NodeContent timeString) = head timeNode --eg: "2013-06-08T14:58:56.591Z"
    time = Data.Time.parseTime defaultTimeLocale
      "%Y-%m-%dT%H:%M:%S %z" ((take 19 (show timeString))++" "++
        (show tzString)) :: Maybe UTCTime

----------------------------
-- Where -------------------
-- -------------------------

{-
makeWhere node = DWhere readPoint bizLocation srcTypes destTypes
  where
    readPoint = getReadPoint node
    bizLocation = getBizLocation node
    srcDestType = getSrcDestTypes node
-}

--returns (Just "urn:epc:id:sgln:0614141.00777.0")
getReadPoint node = if (isNothing readPointNode) || (isNothing ids)
                       then Nothing else Just id
  where
    readPointNode = searchElement "readPoint" node
    (Element _ _ rpNode) = head $ fromJust $ readPointNode
    ids = searchElements "id" rpNode
    (Element _ _ idNodes) = head $ fromJust $ ids
    (NodeContent id) = head idNodes

--getBizLocation :: Node -> Maybe BizLocation
--returns (Just "urn:epc:id:sgln:0614141.00888.0")
getBizLocation node = if isNothing $ bizLocationNode
                         then Nothing else Just id
  where
    bizLocationNode = searchElement "bizLocation" node
    (Element _ _ bizNode) = head $ fromJust $ bizLocationNode
    ids = searchElements "id" bizNode
    (Element _ _ idNodes) = head $ fromJust $ ids
    (NodeContent id) = head idNodes


getSrcTypes :: Node -> Maybe [SourceDestType]
getSrcTypes = error "Implement me"

getDestTypes :: Node -> Maybe [SourceDestType]
getDestTypes = error "Implement me"


----------------------------
-- Utilities ---------------
----------------------------


-- search the node's children for a node with a particular Name
-- searchElement :: Name -> Node -> [Element]
searchElement term node = if result==[] then Nothing else Just result
  where
    result = searchElement' term node

searchElement' term (NodeElement e) =
  if name==term then e:rest else rest
  where
    (Element name _ nodes) = e
    rest = searchElements' term nodes

searchElement' _ (NodeContent t) = []
searchElement' _ (NodeComment _) = []
searchElement' _ (NodeInstruction _) = []

searchElements term nodes = if result==[] then Nothing else Just result
  where
    result = searchElements' term nodes

searchElements' term nodes = (concat $ map (searchElement' term) nodes)


getNames :: Node -> [Name]
getNames (NodeElement (Element name _ nodes)) = name:(concat $ map getNames nodes)
getNames (NodeContent t) = []
getNames (NodeComment _) = []
getNames (NodeInstruction _) = []


{-
createAggregationEvent :: Node -> Event
createAggregationEvent NodeElement e =
  newEvent eventID AggregationEventT what when why whre
    where
      when = getWhen e

getWhen :: Node -> DWhen
getWhen NodeElement e =
  searchElement "eventTime"
-}


