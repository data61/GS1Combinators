{-# LANGUAGE OverloadedStrings #-}

module XML.Parser where

import           Data.GS1.DWhat
import           Data.GS1.EPC
import           Data.GS1.EPCISTime
import           Data.GS1.Event
import           Data.GS1.Location
import           Data.GS1.Utils
import           Data.List.Split
import           Data.Maybe
import qualified Data.Text           as T
import           Data.Time.LocalTime
import           Data.XML.Types
import           Text.Read
import           Text.XML.Cursor

-- |Get all the cursors with the given name below the current cursor
getCursorsByName :: Name -> Cursor -> [Cursor]
getCursorsByName n c = c $// element n

-- |Only the first occurance of EventTime for each Event will be recognised
parseTimeXML :: [T.Text] -> Maybe EPCISTime
parseTimeXML t = case t of
                   (x:_) -> let pt = parseStr2Time (T.unpack x) :: Either EPCISTimeError EPCISTime in
                                 case pt of
                                   Left _  -> Nothing
                                   Right a -> Just a
                   _      -> Nothing

-- |Only the first occurrance of EventTime for each Event will be recognised
parseTimeZoneXML :: [T.Text] -> Maybe TimeZone
parseTimeZoneXML t = case t of
                       (x:_) -> let ptz = parseStr2TimeZone (T.unpack x) :: Either EPCISTimeError TimeZone in
                                     case ptz of
                                       Left _  -> Nothing
                                       Right a -> Just a
                       _      -> Nothing

-- |Parse TimeZone from eventTimeZoneOffset
-- Only the first occured TimeZone will be considered
parseTimeZoneXML' :: [T.Text] -> Maybe TimeZone
parseTimeZoneXML' [] = Nothing
parseTimeZoneXML' (t:_) = let l = splitOn ":" (T.unpack t) in
                              case l of
                                (x:_) -> let rx = readMaybe x :: Maybe Int in
                                             case rx of
                                               Just t'  -> Just $ hoursToTimeZone t'
                                               Nothing  -> Nothing
                                _      -> Nothing

-- |The name of the current cursor stays at ObjectEvent
parseDWhen :: Cursor -> Maybe DWhen
parseDWhen c = do
  let etn = c $/ element "eventTime" &/ content
  let rtn = c $/ element "recordTime" &/ content
  let tzn = c $/ element "eventTimeZoneOffset" &/ content
  let et = parseTimeXML etn
  let rt = parseTimeXML rtn
  let tz = if isNothing $ parseTimeZoneXML' tzn then parseTimeZoneXML' tzn else parseTimeZoneXML etn
  case et of
    Just et' -> Just (DWhen et' rt (fromJust tz))
    _        -> Nothing

parseAction :: [T.Text] -> Maybe Action
parseAction t = case t of
                  (x:_) -> mkAction . T.unpack $ x
                  _     -> Nothing

parseEPCList :: [T.Text] -> [EPC]
parseEPCList ts = fromJust <$> (mkEPC "EPC" . T.unpack <$> ts)

-- |TODO: due to lack of data, source destination type might not be implemented for now
-- there could be multiple readpoints and bizlocations
-- and there could be no srcDest Type involved
-- the sgln could be irregular
-- |TODO: there must be some more modification on it
parseDWhere :: Cursor -> Maybe DWhere
parseDWhere c = do
  let rp = c $/ element "readPoint" &/ element "id" &/ content
  let bl = c $/ element "bizLocation" &/ element "id" &/ content
  let rps = (mkLocation . T.unpack) <$> rp
  let bls = (mkLocation . T.unpack) <$> bl
  Just $ DWhere rps bls [] []
