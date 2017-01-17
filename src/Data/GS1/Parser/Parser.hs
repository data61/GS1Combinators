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
    print prologue
    print "-----------"
    print root
    print "-----------"
    print epilogue
    print "-----------"



