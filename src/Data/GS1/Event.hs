{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.GS1.Event where

import           Control.Lens
import           Data.Either.Combinators
import           Data.GS1.DWhat
import           Data.GS1.DWhy
import           Data.GS1.EPCISTime
import           Data.GS1.EventID
import           Data.GS1.Location
import           Data.GS1.SourceDest
import           Data.GS1.Utils
import           Data.Time
import           GHC.Generics


data DWhen = DWhen
  {
    _eventTime  :: EPCISTime
  , _recordTime :: Maybe EPCISTime -- minOccurs = 0
  , _timeZone   :: TimeZone
  }
  deriving (Show, Eq, Generic)

makeClassy ''DWhen

-- |eventTime, recordTime, the timezone is eventTime's
mkDWhen :: String -> String -> Maybe DWhen
mkDWhen a b = let et = parseStr2Time a :: Either EPCISTimeError EPCISTime
                  rt = parseStr2Time b :: Either EPCISTimeError EPCISTime
                  tz = parseStr2TimeZone a :: Either EPCISTimeError TimeZone in
                  case et of
                    Right et' -> case rt of
                                   Right rt' -> let diff = diffUTCTime et' rt' in
                                                    if diff < 0
                                                    then Just $ DWhen et' (Just rt') (fromRight' tz) else Nothing
                                   _         -> Just $ DWhen et' Nothing (fromRight' tz)
                    _         -> Nothing

-- |use it when event time and record time are the same
mkDWhen' :: String -> Maybe DWhen
mkDWhen' s = let t = parseStr2Time s :: Either EPCISTimeError EPCISTime
                 tz = parseStr2TimeZone s :: Either EPCISTimeError TimeZone in
                 case t of
                   Right t' -> Just $ DWhen t' (Just t') (fromRight' tz)
                   _        -> Nothing


data DWhere = DWhere
  {
    _readPoint   :: Maybe ReadPointLocation
  , _bizLocation :: Maybe BizLocation
  , _srcType     :: [SourceDestType]
  , _destType    :: [SourceDestType]
  }
  deriving (Show, Eq, Generic)

makeClassy ''DWhere

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

mkEvent :: EventID -> EventType -> DWhat -> DWhen -> DWhy -> DWhere -> Maybe Event
mkEvent i t w1 w2 w3 w4 = let e = (Just . Event) $ Eventish t i w1 w2 w3 w4 in
                               case (t, w1) of
                                 (ObjectEventT, ObjectDWhat{})                 -> e
                                 (AggregationEventT, AggregationDWhat{})       -> e
                                 (QuantityEventT, QuantityDWhat{})             -> e
                                 (TransactionEventT, TransactionDWhat{})       -> e
                                 (TransformationEventT, TransformationDWhat{}) -> e
                                 _                                             -> Nothing


