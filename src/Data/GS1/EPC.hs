{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.GS1.EPC where

import           Control.Lens.TH
import           Control.Monad.Error.Lens
import           Control.Monad.Except     (MonadError)
import           Data.Char
import           Data.List
import           GHC.Generics
import           Text.Read

-- |TODO TEMP EpcClass is a String
type EpcClass = String

-- |Elctronic Product Code
-- It could represented by many standards
-- For example GLN (GTIN13) is one of them
-- Currently it has one method to convert the meaningful EPC to a consecutive string
data EPC = GLN GS1CompanyPrefix LocationRef CheckDigit
  deriving (Eq)

-- |FIXME DEBUG Show
instance Show EPC where
  show _gln@(GLN _ _ _) = ppGLN _gln

-- |Pretty print GLN
ppGLN :: EPC -> String
ppGLN (GLN pref ref cd) = intercalate "." [pref, ref, cd]

-- |Assigned by a GS1 Member Organisation to a user/subscriber
type GS1CompanyPrefix = String

-- |Allocated by the company to a specific location
type LocationRef = String

-- |Calculated according to an algorithm https://en.wikipedia.org/wiki/Global_Location_Number
type CheckDigit = String

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

-- | Check the length and isDigit for all chars
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

-- |Creates a valid GLN
gln ::(AsLocationError e, MonadError e m)
     => GS1CompanyPrefix -> LocationRef -> CheckDigit -> m EPC
gln pref ref cd
  | not (wellFormatGLN pref ref cd) = throwing _IllegalFormat ()
  | not (validateGLN pref ref cd)   = throwing _InvalidChecksum ()
  | otherwise                       = pure (GLN pref ref cd)
