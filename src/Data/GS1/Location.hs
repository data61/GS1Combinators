{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.GS1.Location where

import           Data.GS1.EPC
import           Data.GS1.URI
import           Data.List
import           GHC.Generics
import           Text.Printf

-- |Location takes a GLN as its argument
data Location = Location EPC
  deriving (Show, Eq, Generic)

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

-- |non-normative representation - simplest form of RFC5870
ppGeoLocation :: GeoLocation -> String
ppGeoLocation (GeoLocation lat lon) = printf "geo:%f,%f" lat lon

instance Show GeoLocation where
  show = ppGeoLocation

instance URI Location where
  ppURI (Location _gln)      = intercalate ":" ["urn:epc:id", "sgln", (ppGLN _gln)]
  uriPrefix _                = "urn:epc:id"
  uriQuantifier _            = "sgln"
  uriPayload (Location _gln) = ppGLN _gln
