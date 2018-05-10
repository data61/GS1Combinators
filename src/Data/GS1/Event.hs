{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Data.GS1.Event where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhy
import           Data.GS1.EventId
import           Data.GS1.Utils
import           Data.String      (IsString)
import           Data.Swagger
import qualified Data.Text        as T
import           GHC.Generics

data EventType
  -- | When something is created
  = ObjectEventT
  -- | When something gets packaged into a container, or gets unpacked.
  | AggregationEventT
  -- | Some transaction has been made between 2 or more parties
  | TransactionEventT
  -- | Some product has been converted into another
  -- e.g A bucket of Tomatoes became some bottles of Tomato Sauce
  | TransformationEventT
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
getEventType ObjWhat{}       = ObjectEventT
getEventType AggWhat{}       = AggregationEventT
getEventType TransactWhat{}  = TransactionEventT
getEventType TransformWhat{} = TransformationEventT

mkEventType :: T.Text -> Maybe EventType
mkEventType = mkByName

allEventTypes :: [EventType]
allEventTypes = [ObjectEventT ..]

data Event = Event
  {
    _etype :: EventType
  , _eid   :: Maybe EventId -- foreign event id, comes from the XML
  , _what  :: DWhat
  , _when  :: DWhen
  , _why   :: DWhy
  , _where :: DWhere
  }
  deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''Event)
instance ToSchema Event
