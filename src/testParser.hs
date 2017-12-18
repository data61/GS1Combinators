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

import           Control.Applicative


flatten :: [[a]] -> [a]
flatten xs = (\z n -> foldr (flip (foldr z)) n xs) (:) []

main :: IO()
main = do
  let eventName = "AggregationEvent"
  doc <- Text.XML.readFile def "../test/test-xml/AggregationEvent.xml"

  let mainCursor = fromDocument doc
  let eCursors = getCursorsByName eventName mainCursor

  -- let dwhat = head $ parseAggregationDWhat <$> eCursors
  -- print dwhat
  let ev = parseEventByType mainCursor AggregationEventT
  -- ev `shouldBe` ...
  TL.putStrLn . TLE.decodeUtf8 $ encodePretty $ fromRight' ev

--   print $ blahfoo <$> oeCursors -- this shows that parseBizTransaction is bugged - mkBizTransactionType might not work with fromJust

-- blahfoo c = do
--   let texts = c $// element "bizTransaction" &/ content
--   let attrs = foldMap id (c $// element "bizTransaction" &| attribute "type")
--   let z = zip attrs texts
--   parseBizTransactionHelp <$> z
--     where
--       parseBizTransactionHelp (a, b) =
--         mkBizTransactionType (T.unpack . T.strip $ b)
