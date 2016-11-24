{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}

module Data.GS1.Location where

import Control.Exception
import Data.Char
import Data.GS1.GS1Exception
import GHC.Generics

-- |calculate the check digit from gs1company prefix and location reference
--  https://data61.slack.com/files/zzhu/F35T5N1L0/check_digit_calculator.pdf
calcCheckDigit :: GS1CompanyPrefix -> LocationRef -> Int
calcCheckDigit pref ref = getDigit (map digitToInt (pref ++ ref))
                            where getDigit arr
                                    | length arr /= 12 = -1
                                    | otherwise        = 10 - ((sumDigit arr) `mod` 10)
                                        where sumDigit arr2 = case arr2 of
                                                               []       -> 0
                                                               [_]      -> 0
                                                               (a:b:xs) -> (a + b * 3) + (sumDigit xs)

-- |validateGLN
validateGLN :: GS1CompanyPrefix -> LocationRef -> CheckDigit -> Bool
validateGLN pref ref cd = (calcCheckDigit pref ref) == (read cd::Int)

-- |Global Location Number
data GLN = GLN GS1CompanyPrefix LocationRef CheckDigit
  deriving (Eq)

instance Show GLN where
  show (GLN pref ref cd) = pref ++ "." ++ ref ++ "." ++ cd

-- |Creates a GLN with valid format
gln :: GS1CompanyPrefix -> LocationRef -> CheckDigit -> GLN
gln pref ref cd
  | validateGLN pref ref cd = GLN pref ref cd
  | otherwise               = throw (InvalidGLNLengthException "invalid length")

-- |Assigned by a GS1 Member Organisation to a user/subscriber
type GS1CompanyPrefix = String

-- |Allocated by the company to a specific location
type LocationRef = String

-- |Calculated according to an algorithm https://en.wikipedia.org/wiki/Global_Location_Number
type CheckDigit = String

-- |Global Location Number
instance Show Location where
  show (Location n) = "urn:epc:id:sgln:" ++ show n

-- |Location takes a GLN as its argument
data Location = Location GLN
  deriving (Eq, Generic)

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
geoFormatSimple :: GeoLocation -> String
geoFormatSimple (GeoLocation lat lon) = "geo" ++ ":" ++ (show lat) ++ "," ++ (show lon)

instance Show GeoLocation where
  show geo = geoFormatSimple geo

