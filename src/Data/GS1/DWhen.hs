{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.GS1.DWhen (DWhen(..)) where

import           Data.Aeson
import           Data.Aeson.TH
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


$(deriveJSON defaultOptions ''DWhen)
instance ToSchema DWhen
