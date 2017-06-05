{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.GS1.EPC where

import           Control.Lens.TH
import           Control.Monad.Error.Lens
import           Control.Monad.Except     (MonadError)
import           Data.Char
import           Data.Either
import           Data.Either.Combinators  (fromRight')
import           Data.List
import           Data.List.Split
import           GHC.Generics
import           Text.Read
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Swagger

import           Database.SQLite.Simple.ToField
import           Data.ByteString.Char8 (pack)

-- |TODO TEMP EPCClass is a String
newtype EPCClass = EPCClass String
  deriving (Eq, Show, Generic)
$(deriveJSON defaultOptions ''EPCClass)
instance ToSchema EPCClass

-- |TODO more restrictions here in the future
mkEPCClass :: String -> Maybe EPCClass
mkEPCClass x = Just $ EPCClass x

-- |Assigned by a GS1 Member Organisation to a user/subscriber
type GS1CompanyPrefix = String

-- |Allocated by the company to a specific location
type LocationRef = String

-- |Calculated according to an algorithm https://en.wikipedia.org/wiki/Global_Location_Number
type CheckDigit = String

data LocationError
  = IllegalGLNFormat
  | InvalidChecksum
  deriving (Show, Eq, Generic)

makeClassyPrisms ''LocationError


-- |Elctronic Product Code
-- It could represented by many standards
-- For example GLN (GTIN13) is one of them
-- TODO: Currently it has one method to convert the meaningful EPC to a consecutive string
data EPC = EPC String
         | GLN GS1CompanyPrefix LocationRef CheckDigit
         deriving (Eq, Generic, Read)
$(deriveJSON defaultOptions ''EPC)
instance ToSchema EPC

instance Show EPC where
  show = ppEPC

instance ToField EPC where
  toField = toField . pack . show

-- |Pretty print EPC
ppEPC :: EPC -> String
ppEPC epc = case epc of
              GLN pref ref cd -> intercalate "." [pref, ref, cd]
              EPC s           -> s


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
  where _concat a b = length (a ++ b)
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
  | not (wellFormatGLN pref ref cd) = throwing _IllegalGLNFormat ()
  | not (validateGLN pref ref cd)   = throwing _InvalidChecksum ()
  | otherwise                       = pure (GLN pref ref cd)

-- |type -> payload -> Maybe EPC
-- TODO: add more types
mkEPC :: String -> String -> Maybe EPC
mkEPC t p = case t of
              "EPC" -> Just $ EPC p
              "GLN" -> case splitOn "." p of
                         [a, b, c] -> let x = gln a b c :: Either LocationError EPC in
                                          if isRight x then Just (fromRight' x) else Nothing
                         _         -> Nothing
              _     -> Nothing
