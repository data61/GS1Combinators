{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Data.GS1.Event where

import GHC.Generics

import Data.GS1.DWhat
import Data.GS1.DWhen
import Data.GS1.DWhere
import Data.GS1.DWhy
import Data.GS1.EventID
import Data.GS1.Utils
import Data.Aeson
import Data.Aeson.TH
import Data.Swagger
import Data.XML.Types      hiding (Event)

import Data.ByteString.Char8 (pack)
import Database.SQLite.Simple.ToField

data EventType = ObjectEventT
               | AggregationEventT
               | TransactionEventT
               | TransformationEventT
               deriving (Show, Eq, Generic, Enum, Read)

$(deriveJSON defaultOptions ''EventType)
instance ToSchema EventType
instance ToField EventType where
  toField = toField . pack . show

mkEventType :: String -> Maybe EventType
mkEventType = mkByName

allEvents :: [EventType]
allEvents = [(ObjectEventT)..] -- bug in hlint! brackets are not redundant

data Event = Event
  {
    _type  :: EventType
  , _eid   :: Maybe EventID
  , _what  :: DWhat
  , _when  :: DWhen
  , _why   :: DWhy
  , _where :: DWhere
  }
  deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''Event)
instance ToSchema Event
