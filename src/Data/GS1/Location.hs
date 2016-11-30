{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.GS1.Location where

import           Control.Lens.Prism
import           Control.Lens.TH
import           Control.Monad.Error.Lens
import           Control.Monad.Except
import           Control.Exception
import           Data.Char
import           Data.List
import           GHC.Generics
import           Text.Printf

data LocationError
  = GLNInvalid
  deriving (Show, Eq, Generic)

makeClassyPrisms ''LocationError

-- |calculate the check digit from gs1company prefix and location reference
--  https://data61.slack.com/files/zzhu/F35T5N1L0/check_digit_calculator.pdf
calcCheckDigit :: GS1CompanyPrefix -> LocationRef -> Int
calcCheckDigit pref ref = getDigit (map digitToInt (pref ++ ref)) where
  getDigit arr
    | length arr /= 12 = -1
    | otherwise        = 10 - (sumDigit arr `mod` 10)
    where sumDigit arr2 = case arr2 of
                          (a:b:xs) -> a + b * 3 + sumDigit xs
                          _        -> 0

-- |validateGLN
-- TODO: each pref ref could be validated
validateGLN :: GS1CompanyPrefix -> LocationRef -> CheckDigit -> Bool
validateGLN "" _ _      = False
validateGLN _ "" _      = False
validateGLN _ _ ""      = False
validateGLN pref ref cd = calcCheckDigit pref ref == (read cd::Int)

-- |Global Location Number
data GLN = GLN GS1CompanyPrefix LocationRef CheckDigit
  deriving (Eq)

instance Show GLN where
  show (GLN pref ref cd) = intercalate "." [pref, ref, cd]

gln ::(AsLocationError e, MonadError e m)
     => GS1CompanyPrefix -> LocationRef -> CheckDigit -> m GLN
gln pref ref cd
  | validateGLN pref ref cd = pure (GLN pref ref cd)
  | otherwise               = throwing _GLNInvalid ()

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
geoFormatSimple (GeoLocation lat lon) = printf "geo:%f,%f" lat lon

instance Show GeoLocation where
  show = geoFormatSimple

