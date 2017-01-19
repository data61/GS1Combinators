{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.GS1.BizStep where

import           Control.Lens.TH
import           Data.GS1.URI
import           Data.GS1.Utils
import           Data.List.Split
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
                  deriving (Show, Eq, Generic)

makeClassy ''BizStep

ppBizStep :: BizStep -> String
ppBizStep = revertCamelCase . show

mkBizStep :: String -> Maybe BizStep
mkBizStep s = let s' = mkCamelCase s in
                  case s' of
                    "Accepting" -> Just Accepting
                    "Arriving"  -> Just Arriving
                    "Assembling" -> Just Assembling
                    "Collecting" -> Just Collecting
                    "Commissioning" -> Just Commissioning 
                    "Consigning"    -> Just Consigning
                    "CreatingClassInstance" -> Just CreatingClassInstance
                    "CycleCounting" ->  Just CycleCounting
                    "Decommissioning" -> Just Decommissioning
                    "Departing" -> Just Departing
                    "Destroying" -> Just Destroying
                    "Disassembling" -> Just Disassembling
                    "Dispensing" -> Just Dispensing
                    "Encoding" -> Just Encoding
                    "EnteringExiting" -> Just EnteringExiting
                    "Holding" -> Just Holding
                    "Inspecting" -> Just Inspecting
                    "Installing" -> Just Installing
                    "Killing" -> Just Killing
                    "Loading" -> Just Loading
                    "Other"   -> Just Other
                    "Packing" -> Just Packing
                    "Picking" -> Just Picking
                    "Receiving" -> Just Receiving
                    "Removing" -> Just Removing
                    "Repackaging" -> Just Repackaging
                    "Repairing" -> Just Repairing
                    "Replacing" -> Just Replacing
                    "Reserving" -> Just Reserving
                    "RetailSelling" -> Just RetailSelling
                    "Shipping" -> Just Shipping
                    "StagingOutbound" -> Just StagingOutbound
                    "StockTaking" -> Just StockTaking
                    "Stocking" -> Just Stocking
                    "Storing" -> Just Storing
                    "Transporting" -> Just Transporting
                    "Unloading" -> Just Unloading
                    "VoidShipping" -> Just VoidShipping
                    _              -> Nothing


instance URI BizStep where
  uriPrefix _     = "urn:epcglobal:cbv"
  uriQuantifier _ = "bizstep"
  uriPayload      = ppBizStep


parseBizStep :: String -> Maybe BizStep
parseBizStep s = let ws = splitOn ":" s in
                     case ws of
                       ["urn", "epcglobal", "cbv", "bizstep", s] -> mkBizStep s
                       _                                         -> Nothing
