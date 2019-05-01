{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Data.GS1.EventType
  ( EventType(..)
  , allEventTypes
  , stringify
  , withEvent
  ) where

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.String      (IsString)
import           Data.Swagger
import           Data.Text        (unpack)
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
  toJSON = String . stringify

instance FromJSON EventType where
  parseJSON = withText "EventType" $ \case
      "ObjectEvent"         -> pure ObjectEventT
      "AggregationEvent"    -> pure AggregationEventT
      "TransactionEvent"    -> pure TransactionEventT
      "TransformationEvent" -> pure TransformationEventT
      t                     -> fail $ "Invalid Event Type: " <> unpack t

stringify :: IsString a => EventType -> a
stringify ObjectEventT         = "ObjectEvent"
stringify AggregationEventT    = "AggregationEvent"
stringify TransactionEventT    = "TransactionEvent"
stringify TransformationEventT = "TransformationEvent"

allEventTypes :: [EventType]
allEventTypes = [ObjectEventT ..]

withEvent :: (Object -> EventType -> Parser a) -> Value -> Parser a
withEvent f = withObject "Event" $ \o -> f o =<< o .: "isA"
