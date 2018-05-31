module RunApp.Main (run) where

import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty
import           Data.Either              (lefts, rights)
import           Data.GS1.EPC             (ParseFailure)
import           Data.GS1.Event           (Event)
import           Data.GS1.Parser.Parser   (parseAllXMLInDir, parseFile)
import qualified Data.Text.Lazy.Encoding  as TLE
import qualified Data.Text.Lazy.IO        as TL
import           System.Environment       (getArgs)
import           System.Exit              (die)

getFailuresInDir :: IO ()
getFailuresInDir = do
  args <- getArgs
  case args of
    [] -> die "Please provide a directory name as an argument"
    (xmlDir:_) -> do
      allFailures <- parseAllXMLInDir xmlDir
      mapM_ (TL.putStrLn . TLE.decodeUtf8 . encodePretty) (lefts allFailures)

getResultsFromEither :: (Foldable t, ToJSON a) => ([Either ParseFailure Event] -> t a) -> IO ()
getResultsFromEither fromEither = do
  xmlFiles <- getArgs
  allParsedEvents <- mapM parseFile xmlFiles
  mapM_ (TL.putStrLn . TLE.decodeUtf8 . encodePretty) (fromEither $ (concatMap id) allParsedEvents)

-- TODO: cmd args parsing
-- based on args, decide if it should be ``getSuccesses`` or ``getFailures``
run :: IO ()
run = getResultsFromEither lefts
