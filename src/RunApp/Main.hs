module RunApp.Main (run) where

import           Data.Aeson.Encode.Pretty
import           Data.Either
import           Data.GS1.Event           (allEventTypes)
import           Data.GS1.Parser.Parser   (parseEventByType)
import qualified Data.Text.Lazy.Encoding  as TLE
import qualified Data.Text.Lazy.IO        as TL
import           System.Environment
import           Text.XML
import           Text.XML.Cursor

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
