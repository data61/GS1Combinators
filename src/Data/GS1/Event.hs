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

data EventType = ObjectEventT
               -- When something is created
               | AggregationEventT
               -- When something gets packaged into a container, or gets unpacked.
               | TransactionEventT
               -- Some transaction has been made between 2 or more parties
               | TransformationEventT
               -- Some product has been converted into another
               -- e.g A bucket of Tomatoes became some bottles of Tomato Sauce
               deriving (Show, Eq, Generic, Enum, Read)

$(deriveJSON defaultOptions ''EventType)
instance ToSchema EventType

stringify :: IsString a => EventType -> a
stringify ObjectEventT         = "ObjectEvent"
stringify AggregationEventT    = "AggregationEvent"
stringify TransactionEventT    = "TransactionEvent"
stringify TransformationEventT = "TransformationEvent"

-- | Calls the appropriate stringify for a DWhat
getEventType :: DWhat -> EventType
getEventType (ObjectDWhat _ _ ) = ObjectEventT
getEventType (AggregationDWhat _ _ _ ) = AggregationEventT
getEventType (TransactionDWhat _ _ _ _) = TransactionEventT
getEventType (TransformationDWhat _ _ _) = TransformationEventT


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
