module RunApp.Main (run) where

import           Data.Aeson.Encode.Pretty
import           Data.Either              (lefts, rights)
import           Data.GS1.Parser.Parser   (parseAllXMLInDir, parseFile)
import qualified Data.Text.Lazy.Encoding  as TLE
import qualified Data.Text.Lazy.IO        as TL
import           System.Environment       (getArgs)

-- @todo cmd args parsing
getSuccesses :: IO ()
getSuccesses = do
  xmlFiles <- getArgs
  allParsedEvents <- mapM parseFile xmlFiles
  print allParsedEvents
  mapM_ (TL.putStrLn . TLE.decodeUtf8 . encodePretty) (rights $ (concatMap id) allParsedEvents)

getFailures :: IO ()
getFailures = do
  args <- getArgs
  let xmlDir = head args
  allFailures <- parseAllXMLInDir xmlDir
  mapM_ (TL.putStrLn . TLE.decodeUtf8 . encodePretty) (lefts allFailures)

run :: IO ()
run = getFailures
