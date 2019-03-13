{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.GS1.DWhen (DWhen(..)) where

import           Data.Aeson
import           Data.GS1.EPC
import           Data.Swagger
import           Data.Time
import           GHC.Generics

data DWhen = DWhen
  {
    _eventTime  :: EPCISTime
  , _recordTime :: Maybe EPCISTime-- minOccurs = 0
  , _timeZone   :: TimeZone
  }
  deriving (Show, Eq, Generic)

instance FromJSON DWhen where
  parseJSON = withObject "BizLocation" $ \v -> DWhen
    <$> v .: "eventTime"
    <*> v .:? "recordTime"
    <*> v .: "eventTimeZoneOffset"

instance ToJSON DWhen where
  toJSON = error "not implemented yet"

instance ToSchema DWhen
