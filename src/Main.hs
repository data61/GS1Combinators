{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson.Encode.Pretty
import           Data.GS1.Parser.Parser
import           Data.GS1.Event
import           Data.Either
import           Text.XML
import           Text.XML.Cursor
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  doc <- Text.XML.readFile def (head args)

  let mainCursor = fromDocument doc
  
  -- scope for optimization: only call parseEventByType on existent EventTypes
  let allParsedEvents = filter (not. null) $ concat $ parseEventByType mainCursor <$> allEvents
  -- print allParsedEvents
  mapM_ (TL.putStrLn . TLE.decodeUtf8 . encodePretty) (rights allParsedEvents)
