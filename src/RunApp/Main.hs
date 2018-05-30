module RunApp.Main (run) where

import           Data.Aeson.Encode.Pretty
import           Data.Either
import           Data.GS1.Parser.Parser   (parseFile)
import qualified Data.Text.Lazy.Encoding  as TLE
import qualified Data.Text.Lazy.IO        as TL
import           System.Environment

-- @todo cmd args parsing
run :: IO ()
run = do
  xmlFiles <- getArgs
  allParsedEvents <- mapM parseFile xmlFiles
  mapM_ (TL.putStrLn . TLE.decodeUtf8 . encodePretty) (rights $ (concatMap id) allParsedEvents)
