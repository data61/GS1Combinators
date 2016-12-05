{-# LANGUAGE DeriveGeneric #-}

module Data.GS1.URN where

import GHC.Generics
import Data.List
import Data.GS1.Location

type URIPrefix = String

type URIQuantifier = String

type URIPayload = String

data GS1URN = 
  GS1URN {
           _prefix     :: URIPrefix
         , _quantifier :: URIQuantifier
         , _payload    :: URIPayload
         }
         deriving (Eq, Generic)

class URN a where
  ppURN :: a -> String

instance URN GS1URN where
  ppURN (GS1URN pref qt pl) = intercalate ":" (map show [pref, qt, pl])

instance URN Location where
  ppURN (Location gln) = intercalate ":" ["urn:epc:id", "sgln", (ppGLN gln)]
