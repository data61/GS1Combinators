{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}

module Data.GS1.Location where

import GHC.Generics

{--
 - section 8.4.2
 - RFC2141
 - location id
--}
type LocId = String

{--
 - A location is either a ReadPointLocation or a BusinessLocation
 - FIXME: To add more definitions
--}
data Location = ReadPointLocation LocId | BusinessLocation LocId
  deriving (Eq, Generic)

getLocId :: Location -> LocId
getLocId (ReadPointLocation id) = id
getLocId (BusinessLocation id) = id

-- TODO: to implement a proper version with URNNamespace
instance Show Location where
  show location = "urn:URNNamespace:**:loc:" ++ (getLocId location)

data ReadPointLocation = RP LocId
  deriving (Show, Eq, Generic)

data BusinessLocation = BIZ LocId
  deriving (Show, Eq, Generic)

-- GeoLocation

type Latitude = Double

type Longitude = Double

data GeoLocation = GeoLocation Latitude Longitude
  deriving (Eq)

{--
 - non-normative representation
 - simplest form of RFC5870
--}
geoFormatSimple :: GeoLocation -> String
geoFormatSimple (GeoLocation lat lon) = "geo" ++ ":" ++ (show lat) ++ "," ++ (show lon)

instance Show GeoLocation where
  show geo = geoFormatSimple geo

