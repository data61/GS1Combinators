{-# LANGUAGE DeriveGeneric #-}

module Data.GS1.URI where

import GHC.Generics
import Data.List
import Data.GS1.Location

--TODO more restrictions
type URIPrefix = String

type URIQuantifier = String

type URIPayload = String

-- |GS1URI specs
-- Ref: CBV 6
-- TODO reserved URI prefix
data GS1URI = 
  GS1URI {
           _prefix     :: URIPrefix
         , _quantifier :: URIQuantifier
         , _payload    :: URIPayload
         }
         deriving (Eq, Generic)

instance Show GS1URI where
  show = ppURI

-- |Anything that could be converted into URI
class URI a where
  ppURI :: a -> String

instance URI GS1URI where
  ppURI (GS1URI _pref _qt _pl) = intercalate ":" (map show [_pref, _qt, _pl])

instance URI Location where
  ppURI (Location _gln) = intercalate ":" ["urn:epc:id", "sgln", (ppGLN _gln)]
