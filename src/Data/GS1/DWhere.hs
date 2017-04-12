{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.GS1.DWhere where

import           Control.Lens
import           GHC.Generics
import           Text.Printf

import           Data.GS1.EPC
import           Data.GS1.URI
import           Data.GS1.Utils
import           Data.Aeson
import           Data.Aeson.TH

-- |Location takes a GLN as its argument
newtype Location = Location EPC
  deriving (Eq, Generic)
$(deriveJSON defaultOptions ''Location)

instance Show Location where
  show (Location e) = show e

mkLocation :: String -> Location
mkLocation s = Location $ EPC s

-- |Location synonym
type ReadPointLocation = Location

-- |Location synonym
type BizLocation = Location

-- |Latitude is Double
type Latitude = Double

-- |Longitude is Double
type Longitude = Double

-- |GeoLocation
data GeoLocation = GeoLocation Latitude Longitude
  deriving (Eq)
$(deriveJSON defaultOptions ''GeoLocation)

-- |non-normative representation - simplest form of RFC5870
ppGeoLocation :: GeoLocation -> String
ppGeoLocation (GeoLocation lat lon) = printf "geo:%f,%f" lat lon

instance Show GeoLocation where
  show = ppGeoLocation

instance URI Location where
  uriPrefix _             = "urn:epc:id"
  uriQuantifier _         = "sgln"
  uriPayload (Location g) = ppEPC g

-- EPCIS 1.2 section 7.3.5.4 line 1150
-- Example can be found at EPCIS 1.2 section 9.6.2 line [3319..3340]
newtype SourceDestID = SourceDestID String
  deriving (Show, Eq, Generic, Read)
$(deriveJSON defaultOptions ''SourceDestID)

instance URI SourceDestID where
  uriPrefix _                 = "urn:epc:id"
  uriQuantifier _             = "sgln"
  uriPayload (SourceDestID s) = s

mkSourceDestID :: String -> SourceDestID
mkSourceDestID = SourceDestID

parseSourceDestID :: String -> Maybe SourceDestID
parseSourceDestID s = let uri = "urn:epc:id:sgln" in
                          parseURI s uri :: Maybe SourceDestID

-- |SourceDestType
data SourceDestType = SDOwningParty
                    | SDProcessingParty
                    | SDLocation
                    deriving (Show, Eq, Generic, Read)
$(deriveJSON defaultOptions ''SourceDestType)

instance URI SourceDestType where
  uriPrefix _     = "urn:epcglobal:cbv"
  uriQuantifier _ = "sdt"
  uriPayload a    = case a of
                      SDOwningParty     -> "owning_party"
                      SDProcessingParty -> "processing_party"
                      SDLocation        -> "location"

mkSourceDestType :: String -> Maybe SourceDestType
mkSourceDestType = mkByName

parseSourceDestType :: String -> Maybe SourceDestType
parseSourceDestType s = let uri = "urn:epcglobal:cbv:sdt" in
                            parseURI s uri :: Maybe SourceDestType

data Source = Source SourceDestType SourceDestID
  deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''Source)

data Destination = Destination SourceDestType SourceDestID
  deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''Destination)

data DWhere = DWhere
  {
    _readPoint   :: [ReadPointLocation]
  , _bizLocation :: [BizLocation]
  , _srcType     :: [SourceDestType]
  , _destType    :: [SourceDestType]
  }
  deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''DWhere)
makeClassy ''DWhere
