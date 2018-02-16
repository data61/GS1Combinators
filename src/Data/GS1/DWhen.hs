{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | module for the When dimension
-- just contains a DWhen type

module Data.GS1.DWhen where

import Data.Time
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Data.Swagger
import Data.GS1.EPC

data DWhen = DWhen
  {
    _eventTime  :: EPCISTime
  , _recordTime :: Maybe EPCISTime-- minOccurs = 0
  , _timeZone   :: TimeZone
  }
  deriving (Show, Eq, Generic)


$(deriveJSON defaultOptions ''DWhen)
instance ToSchema DWhen
