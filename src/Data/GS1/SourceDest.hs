{-# LANGUAGE DeriveGeneric #-}

module Data.GS1.SourceDest where

import           Data.GS1.URI
import           GHC.Generics

-- EPCIS 1.2 section 7.3.5.4 line 1150
-- Example can be found at EPCIS 1.2 section 9.6.2 line [3319..3340]
data SourceDestID = SourceDestID String
  deriving (Show, Eq, Generic)

instance URI SourceDestID where
  uriPrefix _                 = "urn:epc:id"
  uriQuantifier _             = "sgln"
  uriPayload (SourceDestID s) = s

data SourceDestType = SDOwningParty
                    | SDProcessingParty
                    | SDLocation
  deriving (Show, Eq, Generic)

instance URI SourceDestType where
  uriPrefix _     = "urn:epcglobal:cbv"
  uriQuantifier _ = "sdt"
  uriPayload a    = case a of
                      SDOwningParty     -> "owning_party"
                      SDProcessingParty -> "processing_party"
                      SDLocation        -> "location"

data Source = Source SourceDestType SourceDestID
  deriving (Show, Eq, Generic)

data Destination = Destination SourceDestType SourceDestID
  deriving (Show, Eq, Generic)
