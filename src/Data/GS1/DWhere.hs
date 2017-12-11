{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.GS1.DWhere where

import           Control.Lens
import           GHC.Generics

import           Data.GS1.EPC
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Swagger

import           Database.SQLite.Simple.ToField
import           Data.Aeson.Text
import qualified Data.Text.Lazy as TxtL

-- |Location synonym
type ReadPointLocation = LocationEPC

-- |Location synonym
type BizLocation = LocationEPC



-- EPCIS 1.2 section 7.3.5.4 line 1150
-- Example can be found at EPCIS 1.2 section 9.6.2 line [3319..3340]
{-
newtype SourceDestID = SourceDestID String
  deriving (Show, Eq, Generic, Read)
$(deriveJSON defaultOptions ''SourceDestID)
instance ToSchema SourceDestID

instance URI SourceDestID where
  uriPrefix _                 = "urn:epc:id"
  uriQuantifier _             = "sgln"
  uriPayload (SourceDestID s) = s

mkSourceDestID :: String -> SourceDestID
mkSourceDestID = SourceDestID

parseSourceDestID :: String -> Maybe SourceDestID
parseSourceDestID s = let uri = "urn:epc:id:sgln" in
                          parseURI s uri :: Maybe SourceDestID
-}


data DWhere = DWhere
  {
    _readPoint   :: [ReadPointLocation]
  , _bizLocation :: [BizLocation]
  , _srcType     :: [(SourceDestType, LocationEPC)]
  , _destType    :: [(SourceDestType, LocationEPC)]
  }
  deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''DWhere)
instance ToSchema DWhere
makeClassy ''DWhere

instance ToField DWhere where
  toField = toField . TxtL.toStrict . encodeToLazyText
