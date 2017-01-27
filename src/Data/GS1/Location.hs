{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.GS1.Location where

import           Data.GS1.EPC
import           Data.GS1.URI
import           GHC.Generics
import           Text.Printf

-- |Location takes a GLN as its argument
newtype Location = Location EPC
  deriving (Eq, Generic)

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

-- |non-normative representation - simplest form of RFC5870
ppGeoLocation :: GeoLocation -> String
ppGeoLocation (GeoLocation lat lon) = printf "geo:%f,%f" lat lon

instance Show GeoLocation where
  show = ppGeoLocation

instance URI Location where
  uriPrefix _                = "urn:epc:id"
  uriQuantifier _            = "sgln"
  uriPayload (Location _gln) = ppGLN _gln
