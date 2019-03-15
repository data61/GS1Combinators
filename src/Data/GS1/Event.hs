{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Data.GS1.Event
  (EventType(..)
  , Event(..)
  , allEventTypes
  , getEventType
  , stringify
  )
  where

import           Data.Aeson
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

instance ToSchema EventType

instance ToJSON EventType where
  toJSON evType = object
    [ "isA" .= (stringify evType :: String) ]

instance FromJSON EventType where
  parseJSON = withText "EventType" $ \case
      "ObjectEvent"         -> pure ObjectEventT
      "AggregationEvent"    -> pure AggregationEventT
      "TransactionEvent"    -> pure TransactionEventT
      "TransformationEvent" -> pure TransformationEventT
      t                     -> fail $ "Invalid Event Type: " <> T.unpack t

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
instance ToSchema Event

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o -> Event
    <$> o .: "isA"
    <*> o .:? "eventID"
    <*> error "lol"
    <*> error "lol"
    <*> error "lol"
    <*> error "lol"
