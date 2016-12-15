{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.GS1.Event where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.Error.Lens
import           Control.Monad.Except     (MonadError)

import           Data.GS1.BizStep
import           Data.GS1.BizTransaction
import           Data.GS1.Disposition
import           Data.GS1.EPCISTime
import           Data.GS1.EventID
import           Data.GS1.Location
import           Data.GS1.Object
import           Data.GS1.SourceDest
import           Data.GS1.URI
import           Data.GS1.Utils
import           Data.GS1.Why
import           Data.List
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.LocalTime
import           GHC.Generics

--TODO
type TransformationID = String -- FIXME user defined element

data When = When 
  {
    _eventTime  :: EPCISTime
  , _recordTime :: EPCISTime
  , _timeZone   :: TimeZone
  }
  deriving (Show, Eq, Generic)

data Where = Where 
  {
    _readPoint   :: (Maybe ReadPointLocation)
  , _bizLocation :: (Maybe BizLocation)
  , _srcDestType :: (Maybe [SourceDestType])
  }
  deriving (Show, Eq, Generic)

data What = ObjectWhat 
  {
    _objects :: [ObjectID]
  , _action  :: Action
  , _btt     :: [BizTransactionType]
  , _ilmd    :: Maybe Ilmd
  } 
  | AggregationWhat 
  {
    _parentID :: Maybe ObjectID
  , _objects  :: [ObjectID]
  , _action   :: Action
  , _btt      :: [BizTransactionType]
  }
  | QuantityWhat
  {
    _objects :: [ObjectID]
  , _btt     :: [BizTransactionType]
  }
  | TransformationWhat
  {
    _input            :: [ObjectID]
  , _output           :: [ObjectID]
  , _transformationID :: TransformationID
  , _btt              :: [BizTransactionType]
  , _ilmd             :: Maybe Ilmd
  }
  | TransactionWhat
  {
    _parentID :: Maybe ObjectID
  , _objects  :: [ObjectID]
  , _action   :: Action
  , _btt      :: [BizTransactionType]
  }
  deriving (Show, Eq, Generic)

data EventType = ObjectEvent
               | AggregationEvent
               | QuantityEvent
               | TransactionEvent
               | TransformationEvent
               deriving (Show, Eq, Generic)

--type EventID = Int --FIXME - user defined element

data Action = Add
            | Observe
            | Delete
            deriving (Show, Eq, Generic)

data Event = Event
  {
    _type  :: EventType
  , _id    :: EventID
  , _what  :: What
  , _when  :: When
  , _why   :: Why
  , _where :: Where
  }
  deriving (Show, Eq, Generic)

instance HasWhy Event where
  why = 
    lens
    (\(Event _ _ _ _ w _) -> w)
    (\(Event t i w1 w2 _ w4) w3 -> Event t i w1 w2 w3 w4)

objectEvent :: EventID -> [ObjectID] -> Action -> [BizTransactionType] ->
    Maybe Ilmd ->When -> Why -> Where -> Event
objectEvent id objects action btt ilmd when why whre =
  Event ObjectEvent id (ObjectWhat objects action btt ilmd) when why whre

--TODO: check parent is present when needed (based on action)
aggregationEvent :: EventID -> Maybe ObjectID -> [ObjectID] -> Action ->
  [BizTransactionType] -> When -> Why -> Where -> Event
aggregationEvent id parent objects action btt when why whre =
  Event AggregationEvent id (AggregationWhat parent objects action btt) when why whre

--TODO: check that all ObjectIDs are class objects with quantities.
quantityEvent :: EventID -> [ObjectID] -> [BizTransactionType] ->
  When -> Why -> Where -> Event
quantityEvent id objects btt when why whre=
  Event QuantityEvent id (QuantityWhat objects btt) when why whre

transformationEvent :: EventID -> [ObjectID] -> [ObjectID] -> TransformationID
                    -> [BizTransactionType] -> Maybe Ilmd -> When -> Why ->
                      Where -> Event
transformationEvent id inputs outputs transformID btt  ilmd when why whre =
  Event TransformationEvent id (TransformationWhat inputs outputs transformID btt ilmd)
    when why whre

transactionEvent :: EventID -> Maybe ObjectID -> [ObjectID] -> Action ->
  [BizTransactionType] -> When -> Why -> Where -> Event
transactionEvent id parentID objects action btt when why whre =
  Event TransactionEvent id (TransactionWhat parentID objects action btt) when why whre

{--
== Object Event ==
 An ObjectEvent Captures information about an event pertaining to one or more physical or digital objects identified by instance-level (EPC) or class-level (EPC Class) identifiers.
 While more than one EPC and/or EPC Class may appear in an ObjectEvent, no relationship or association between those objects i implied other than the coincidence of having experienced identical events in the real world
*denotes optional field
 - Fields -
 When - EventTime/RecordTime/EventTimeOffset
 What - EpcList* quantityList* (an ObjectEvent shall contain a non-empty epcList or a non-empty quantityList or both)
        Action
        Ilmd
        [BizTransactionType]*
 Why -
        BizStep*
        Disposition*

 Where -
        ReadPoint*
        BizLocation*
        [SrcDestType]*
--}

{--
== Aggregation Event ==

 - Fields -
 When - EventTime/RecordTime/EventTimeOffset
 What -
        ParentID** (Optional when Action == Observe, required otherwise)
        [ChildEPC]* (An AggregationEvent SHALL contain either a non-empty [childEPCs] a non-empty [childQuantity], or both, except that both may be empty if Action==Delete)
        [ChildQuantity]*
        Action
        [BizTransactionType]
 Why -
        BizStep*
        Disposition*
 Where -
        ReadPoint*
        BizLocation*
        [SrcDestType]
--}

{--
== Quantity Event ==
 - Fields -
 When - EventTime/RecordTime/EventTimeOffset
 What -
        EPCClass, Quantity
        [BizTransactionType]
 Why -
        BizStep*
        Disposition*
 Where -
        ReadPoint*
        BizLocation*
--}

{--
== Transaction Events ==
 - Fields -
 When - EventTime/RecordTime/EventTimeOffset
 What -
        [BizTransactionType]
        ParentID*
        EPCList, Quantity List
        Action
 Why -
        BizStep
        Disposition

 Where -
        ReadPoint
        BizLocation
        [SrcDestType]
--}

{--
== Transformation Events ==
 - Fields -
 When - EventTime/RecordTime/EventTimeOffset
 What - [InputEPC]*, [OutputQuantity]*
        [OutputEPC]*, [OutputQuantity]*
        TransformationID*
        [BizTransactionType]
        Ilmd*
 Why  -
        BizStep*
        Disposition*
 Where -
        ReadPoint*
        BizLocation*
        [SrcDestType]*
--}

