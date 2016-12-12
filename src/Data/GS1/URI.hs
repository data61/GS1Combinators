module Data.GS1.URI where

import           Data.List

-- More Refernce: TDS 1.9

-- URI Prefix
type URIPrefix = String

-- URI Quantifier
type URIQuantifier = String

-- URI Payload
type URIPayload = String

-- |Anything that could be converted into URI
class URI a where
  ppURI         :: a -> String
  uriPrefix     :: a -> URIPrefix
  uriQuantifier :: a -> URIQuantifier
  uriPayload    :: a -> URIPayload

-- |All CBV Compatible elements are URI
--class URI a => CBVCompatible a where
--  cbv :: a -> String  -- indicate CBV compatible

-- |All CBV Compliant elements are CBV Compatible
--class CBVCompliance a => CBVCompatible a
