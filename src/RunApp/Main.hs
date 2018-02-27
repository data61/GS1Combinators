{-# LANGUAGE OverloadedStrings #-}

-- | contains the main function if we want to run the app
-- to use:
-- stack exec Parser-exe -- <sample xml file>

module RunApp.Main (run) where

import           Data.Aeson.Encode.Pretty
import           Data.GS1.Parser.Parser (parseEventByType)
import           Data.GS1.Event (allEventTypes)
import           Data.Either
import           Text.XML
import           Text.XML.Cursor
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           System.Environment

run :: IO ()
run = do
  args <- getArgs
  -- @todo cmd args parsing
  doc <- Text.XML.readFile def (head args)
  let mainCursor = fromDocument doc
  -- scope for optimization: only call parseEventByType on existent EventTypes
      allParsedEvents =
        filter (not . null) $ concat $
        parseEventByType mainCursor <$> allEventTypes

  mapM_ (TL.putStrLn . TLE.decodeUtf8 . encodePretty) (rights allParsedEvents)
