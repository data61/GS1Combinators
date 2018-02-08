{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedStrings     #-}

module Data.GS1.Event where

import            GHC.Generics
import            Data.GS1.DWhat
import            Data.GS1.DWhen
import            Data.GS1.DWhere
import            Data.GS1.DWhy
import            Data.GS1.EventID
import            Data.GS1.Utils
import            Data.Aeson
import qualified  Data.Text as T
import            Data.String (IsString)
import            Data.Aeson.TH
import            Data.Swagger
import            Data.ByteString.Char8 (pack)
import            Database.SQLite.Simple.ToField


data EventType = ObjectEventT
               | AggregationEventT
               | TransactionEventT
               | TransformationEventT
               deriving (Show, Eq, Generic, Enum, Read)

$(deriveJSON defaultOptions ''EventType)
instance ToSchema EventType
instance ToField EventType where
  toField = toField . pack . show

evTypeToTextLike :: IsString a => EventType -> a
evTypeToTextLike ObjectEventT         = "ObjectEvent"
evTypeToTextLike AggregationEventT    = "AggregationEvent"
evTypeToTextLike TransactionEventT    = "TransactionEvent"
evTypeToTextLike TransformationEventT = "TransformationEvent"

-- | Calls the appropriate evTypeToTextLike for a DWhat
dwhatToEventTextLike :: IsString a => DWhat -> a
dwhatToEventTextLike (ObjectDWhat _ _ ) = evTypeToTextLike ObjectEventT
dwhatToEventTextLike (AggregationDWhat _ _ _ ) = evTypeToTextLike AggregationEventT
dwhatToEventTextLike (TransactionDWhat _ _ _ _) = evTypeToTextLike TransactionEventT
dwhatToEventTextLike (TransformationDWhat _ _ _) = evTypeToTextLike TransformationEventT


mkEventType :: T.Text -> Maybe EventType
mkEventType = mkByName

allEventTypes :: [EventType]
allEventTypes = [(ObjectEventT)..] -- bug in hlint! brackets are not redundant

data Event = Event
  {
    _type  :: EventType
  , _eid   :: Maybe EventID -- foreign event id, comes from the XML
  , _what  :: DWhat
  , _when  :: DWhen
  , _why   :: DWhy
  , _where :: DWhere
  }
  deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''Event)
instance ToSchema Event
