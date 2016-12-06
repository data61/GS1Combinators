{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.GS1.Location where

import Data.GS1.EPC
import GHC.Generics
import Text.Printf

-- |Location takes a GLN as its argument
data Location = Location EPC
  deriving (Show, Eq, Generic)

-- |Location synonym
type ReadPointLocation = Location

-- |Location synonym
type BusinessLocation = Location

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
