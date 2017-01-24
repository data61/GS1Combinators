{-# LANGUAGE OverloadedStrings #-}

module XML.Parser where

import           Data.GS1.EPCISTime
import           Data.GS1.Event
import           Data.GS1.Utils
import           Data.Maybe
import qualified Data.Text           as T
import           Data.Time.LocalTime
import           Text.XML.Cursor

-- Only the first occurance of EventTime for each Event will be recognised
parseTimeXML :: [T.Text] -> Maybe EPCISTime
parseTimeXML t = case t of
                   (x:_) -> let pt = parseStr2Time (T.unpack x) :: Either EPCISTimeError EPCISTime in
                                 case pt of
                                   Left _  -> Nothing
                                   Right a -> Just a
                   _      -> Nothing

parseTimeZoneXML :: [T.Text] -> Maybe TimeZone
parseTimeZoneXML t = case t of
                       (x:_) -> let ptz = parseStr2TimeZone (T.unpack x) :: Either EPCISTimeError TimeZone in
                                     case ptz of
                                       Left _  -> Nothing
                                       Right a -> Just a
                       _      -> Nothing

-- | the name of the current cursor stays at ObjectEvent
parseDWhen :: Cursor -> Maybe DWhen
parseDWhen c = do
  let etn = c $/ element "eventTime" &/ content
  let rtn = c $/ element "recordTime" &/ content
  let et = parseTimeXML etn
  let rt = parseTimeXML rtn
  let tz = parseTimeZoneXML etn
  case et of
    Just et' -> Just (DWhen et' rt (fromJust tz))
    _        -> Nothing

objectEventCursors :: Cursor -> [Cursor]
objectEventCursors c = c $// element "ObjectEvent"
