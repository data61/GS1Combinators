{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.GS1.Event where

import           Control.Lens
import           GHC.Generics

import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhy
import           Data.GS1.EventID
import           Data.GS1.Utils

data EventType = ObjectEventT
               | AggregationEventT
               | QuantityEventT
               | TransactionEventT
               | TransformationEventT
               deriving (Show, Eq, Generic, Read)

mkEventType :: String -> Maybe EventType
mkEventType = mkByName

data Eventish a = Eventish
  {
    _type  :: a
  , _eid   :: EventID
  , _what  :: DWhat
  , _when  :: DWhen
  , _why   :: DWhy
  , _where :: DWhere
  }
  deriving (Show, Eq, Generic)

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

mkEvent :: EventType -> EventID -> DWhat -> DWhen -> DWhy -> DWhere -> Maybe Event
mkEvent t i w1 w2 w3 w4 = let e = (Just . Event) $ Eventish t i w1 w2 w3 w4 in
                              case (t, w1) of
                                (ObjectEventT, ObjectDWhat{})                 -> e
                                (AggregationEventT, AggregationDWhat{})       -> e
                                (QuantityEventT, QuantityDWhat{})             -> e
                                (TransactionEventT, TransactionDWhat{})       -> e
                                (TransformationEventT, TransformationDWhat{}) -> e
                                _                                             -> Nothing
