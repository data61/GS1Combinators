module Data.GS1.Parser.Parser where


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import           Prelude         hiding (readFile, writeFile)
import qualified Data.Map        as M
--import           Text.Hamlet.XML
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





getEventList :: Element -> [Node]
getEventList (Element _ _ nodes) = filter isElement events
  where
    (NodeElement (Element _ _ bodyNodes)) = head(filter isElement nodes)
    (NodeElement (Element _ _ events))= head(filter isElement bodyNodes)



isElement :: Node -> Bool
isElement (NodeElement e) = True
isElement _ = False



