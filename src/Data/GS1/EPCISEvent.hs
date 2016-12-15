{-# LANGUAGE DeriveGeneric #-}

module Data.GS1.EPCISEvent where

import           Data.GS1.ErrorDeclaration (ErrorDeclaration)
import           Data.GS1.EventID
import           GHC.Generics

-- |EPCIS 7.4.1
data EPCISEvent = EPCISEvent
  {
    _eventTime           :: EPCISTime
  , _recordTime          :: Maybe EPCISTime
  , _eventTimeZoneOffset :: String
  , _eventID             :: Maybe EventID
  , _errorDeclaration    :: ErrorDeclaration
  }
