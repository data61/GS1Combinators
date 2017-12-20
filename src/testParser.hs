{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson.Encode.Pretty
import           Data.GS1.Parser.Parser
import           Data.Maybe
import           Data.Either
import           Data.Either.Combinators
import qualified Data.Text as T
import           Text.XML
import           Text.XML.Cursor
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.GS1.EPC
import           Data.GS1.DWhere
import           Data.GS1.DWhat
import           Data.GS1.Event
import           Data.GS1.Utils
import           Control.Applicative


main :: IO ()
main = do
  doc <- Text.XML.readFile def "../test/test-xml/ObjectEvent.xml"

  let mainCursor = fromDocument doc
  
  -- scope for optimization: only call parseEventByType on existent EventTypes
  let allParsedEvents = filter (not. null) $ concat $ parseEventByType mainCursor <$> allEvents
  -- print allParsedEvents
  mapM_ (TL.putStrLn . TLE.decodeUtf8 . encodePretty) (rights allParsedEvents)
