{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Data.GS1.Event where

-- import           Control.Lens
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

import Data.ByteString.Char8 (pack)
import Database.SQLite.Simple.ToField

data EventType = ObjectEventT
               | AggregationEventT
               | TransactionEventT
               | TransformationEventT
               deriving (Show, Eq, Generic, Read)
$(deriveJSON defaultOptions ''EventType)
instance ToSchema EventType
instance ToField EventType where
  toField = toField . pack . show


mkEventType :: String -> Maybe EventType
mkEventType = mkByName

data Event = Event
  {
    _type  :: EventType
  , _eid   :: EventID
  , _what  :: DWhat
  , _when  :: DWhen
  , _why   :: DWhy
  , _where :: DWhere
  }
  deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''Event)
instance ToSchema Event

{-
  --  commenting out the lens stuff, we don't use it anyway...
instance HasEventID (Eventish a) where
  eventID =
    lens
    (\(Eventish _ i _ _ _ _) -> i)
    (\(Eventish t _ w1 w2 w3 w4) i -> Eventish t i w1 w2 w3 w4)

instance HasDWhat (Eventish a) where
  dWhat =
    lens
    (\(Eventish _ _ w _ _ _) -> w)
    (\(Eventish t i _ w2 w3 w4) w1 -> Eventish t i w1 w2 w3 w4)

instance HasDWhen (Eventish a) where
  dWhen =
    lens
    (\(Eventish _ _ _ w _ _ ) -> w)
    (\(Eventish t i w1 _ w3 w4) w2 -> Eventish t i w1 w2 w3 w4)

instance HasDWhy (Eventish a) where
  dWhy =
    lens
    (\(Eventish _ _ _ _ w _) -> w)
    (\(Eventish t i w1 w2 _ w4) w3 -> Eventish t i w1 w2 w3 w4)

instance HasDWhere (Eventish a) where
  dWhere =
    lens
    (\(Eventish _ _ _ _ _ w) -> w)
    (\(Eventish t i w1 w2 w3 _) w4 -> Eventish t i w1 w2 w3 w4)

newtype Event = Event (Eventish EventType)
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions ''Event)
-}
