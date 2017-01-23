{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.GS1.BizStep where

import           Control.Lens.TH
import           Data.GS1.URI
import           Data.GS1.Utils
import           GHC.Generics

data BizStep = Accepting
                  | Arriving
                  | Assembling
                  | Collecting
                  | Commissioning
                  | Consigning
                  | CreatingClassInstance
                  | CycleCounting
                  | Decommissioning
                  | Departing
                  | Destroying
                  | Disassembling
                  | Dispensing
                  | Encoding
                  | EnteringExiting
                  | Holding
                  | Inspecting
                  | Installing
                  | Killing
                  | Loading
                  | Other
                  | Packing
                  | Picking
                  | Receiving
                  | Removing
                  | Repackaging
                  | Repairing
                  | Replacing
                  | Reserving
                  | RetailSelling
                  | Shipping
                  | StagingOutbound
                  | StockTaking
                  | Stocking
                  | Storing
                  | Transporting
                  | Unloading
                  | VoidShipping
                  deriving (Show, Eq, Generic, Read)

makeClassy ''BizStep

ppBizStep :: BizStep -> String
ppBizStep = revertCamelCase . show

mkBizStep :: String -> Maybe BizStep
mkBizStep = mkByName

instance URI BizStep where
  uriPrefix _     = "urn:epcglobal:cbv"
  uriQuantifier _ = "bizstep"
  uriPayload      = ppBizStep

parseBizStep :: String -> Maybe BizStep
parseBizStep s  = let uri = "urn:epcglobal:cbv:bizstep" in
                      parseURI s uri :: Maybe BizStep
