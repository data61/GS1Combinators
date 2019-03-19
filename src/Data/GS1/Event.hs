{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Data.GS1.Event
  ( Event(..)
  , EventType(..)
  , getEventType
  )
  where

import Data.Aeson
import Data.GS1.DWhat
import Data.GS1.DWhen
import Data.GS1.DWhere
import Data.GS1.DWhy
import Data.GS1.EventId
import Data.GS1.EventType ( EventType(..), withEvent )
import Data.Swagger
import GHC.Generics



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
  parseJSON = withEvent $ \o eType ->
    Event eType <$> o .:? "eventID"
                <*> parseJSON (Object o)
                <*> parseJSON (Object o)
                <*> parseJSON (Object o)
                <*> parseJSON (Object o)

instance ToJSON Event where
  toJSON (Event t eif _what _when _why _where) =
    let o = object [ "isA" .= t , "eventID" .= eif ] in
      foldr merge o [ toJSON _what
                    , toJSON _when
                    , toJSON _why
                    , toJSON _where
                    ]
       
    where
      merge :: Value -> Value -> Value
      merge (Object a) (Object b) = Object (a <> b)
      merge a _ = a

-- | Calls the appropriate stringify for a DWhat
getEventType :: DWhat -> EventType
getEventType ObjWhat{}       = ObjectEventT
getEventType AggWhat{}       = AggregationEventT
getEventType TransactWhat{}  = TransactionEventT
getEventType TransformWhat{} = TransformationEventT
