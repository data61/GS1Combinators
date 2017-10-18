module Data.GS1.URI where
{-

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
  ppURI a = intercalate ":" (fmap ($ a) [uriPrefix, uriQuantifier, uriPayload])
  uriPrefix     :: a -> URIPrefix
  uriQuantifier :: a -> URIQuantifier
  uriPayload    :: a -> URIPayload
  -}
