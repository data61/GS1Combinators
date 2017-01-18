{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.GS1.Parser.Parser where
import           Prelude         hiding (readFile, writeFile)
import Data.Time
import Data.Time.Format

import qualified Data.Map        as M
import           Text.XML
import Data.GS1.Event

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
    let timeZoneOffset = searchElement "eventTime" (head eventList)
    print "timezoneoffset: -------------"
    print timeZoneOffset
    print "------------------"
    let when = makeWhen (head eventList)
    print when
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

--makeWhen :: Node -> (Text, Text)
makeWhen node  = (timeString, tzString)
  where
    timeElements = searchElement "eventTime" node
    timeZoneElements = searchElement "eventTimeZoneOffset" node
    (Element _ _ tzNode) = head timeZoneElements
    (NodeContent tzString) = head tzNode --eg: "+02:00"
    (Element _ _ timeNode) = head timeElements
    (NodeContent timeString) = head timeNode --eg: "2013-06-08T14:58:56.591Z"
    time = Data.Time.parseTime defaultTimeLocale
      "%Y-%m-%dT%H:%M:%S %z" ((take 19 (show timeString))++" "++
        (show tzString)) :: Maybe UTCTime



-- search the node's children for a node with a particular Name
-- searchElement :: Name -> Node -> [Element]
searchElement term (NodeElement e) =
  if name==term then e:rest else rest
  where
    (Element name _ nodes) = e
    rest = (concat $ map (searchElement term) nodes)

searchElement _ (NodeContent t) = []
searchElement _ (NodeComment _) = []
searchElement _ (NodeInstruction _) = []





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


