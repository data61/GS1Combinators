{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.GS1.Location where

import           Control.Lens.TH
import           Control.Monad.Error.Lens
import           Control.Monad.Except
import           Data.Char
import           Data.List
import           GHC.Generics
import           Text.Printf
import           Text.Read

data LocationError
  = IllegalFormat
  | InvalidChecksum
  deriving (Show, Eq, Generic)

makeClassyPrisms ''LocationError

-- |calculate the check digit from gs1company prefix and location reference
--  https://data61.slack.com/files/zzhu/F35T5N1L0/check_digit_calculator.pdf
calcCheckDigit :: GS1CompanyPrefix -> LocationRef -> Int
calcCheckDigit pref ref = getDigit (map digitToInt (pref ++ ref)) where
  getDigit arr = 10 - (sumDigit arr `mod` 10)
    where sumDigit arr2 = case arr2 of
                          (a:b:xs) -> a + b * 3 + sumDigit xs
                          _        -> 0

wellFormatGLN :: GS1CompanyPrefix -> LocationRef -> CheckDigit -> Bool
wellFormatGLN pref ref cd = _concat pref ref == 12 && length cd == 1 && _isNum pref ref cd
  where _concat a b = length (concat [a, b])
        _isNum a b c = 
          let mint = readMaybe (concat [a, b, c]) :: Maybe Integer in
          case mint of
            Just _  -> True
            Nothing -> False

-- |validateGLN
validateGLN :: GS1CompanyPrefix -> LocationRef -> CheckDigit -> Bool
validateGLN pref ref cd = calcCheckDigit pref ref == (read cd::Int)

-- |Global Location Number
data GLN = GLN GS1CompanyPrefix LocationRef CheckDigit
  deriving (Eq)

-- |Pretty print GLN
ppGLN :: GLN -> String
ppGLN (GLN pref ref cd) = intercalate "." [pref, ref, cd]

instance Show GLN where
  show _gln = ppGLN _gln

-- |Creates a valid GLN
gln ::(AsLocationError e, MonadError e m)
     => GS1CompanyPrefix -> LocationRef -> CheckDigit -> m GLN
gln pref ref cd
  | not (wellFormatGLN pref ref cd) = throwing _IllegalFormat ()
  | not (validateGLN pref ref cd)   = throwing _InvalidChecksum ()
  | otherwise                       = pure (GLN pref ref cd)

-- |Assigned by a GS1 Member Organisation to a user/subscriber
type GS1CompanyPrefix = String

-- |Allocated by the company to a specific location
type LocationRef = String

-- |Calculated according to an algorithm https://en.wikipedia.org/wiki/Global_Location_Number
type CheckDigit = String

-- |Location takes a GLN as its argument
data Location = Location GLN
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

