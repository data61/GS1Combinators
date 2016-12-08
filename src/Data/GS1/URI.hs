{-# LANGUAGE DeriveGeneric #-}

module Data.GS1.URI where

import           Data.List
import           GHC.Generics

-- TDS 1.9
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
  ppURI         :: a -> String
  uriPrefix     :: a -> URIPrefix
  uriQuantifier :: a -> URIQuantifier
  uriPayload    :: a -> URIPayload

instance URI GS1URI where
  ppURI (GS1URI _pref _qt _pl)   = intercalate ":" (map show [_pref, _qt, _pl])
  uriPrefix (GS1URI _pref _ _)   = _pref
  uriQuantifier (GS1URI _ _qt _) = _qt
  uriPayload (GS1URI _ _ _pl)     = _pl
