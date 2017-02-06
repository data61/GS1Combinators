{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.GS1.DWhy where

import           Control.Lens
import           Data.Maybe
import           GHC.Generics

import           Data.GS1.URI
import           Data.GS1.Utils

-- | BizStep
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

mkBizStep' :: String -> Maybe BizStep
mkBizStep' = mkByName

instance URI BizStep where
  uriPrefix _     = "urn:epcglobal:cbv"
  uriQuantifier _ = "bizstep"
  uriPayload      = ppBizStep

mkBizStep :: String -> Maybe BizStep
mkBizStep s  = let uri = "urn:epcglobal:cbv:bizstep" in
                      parseURI s uri :: Maybe BizStep

data DispositionError = InvalidDisposition
                      | OtherDispositionError
                      deriving (Show, Eq, Generic)

makeClassyPrisms ''DispositionError

data Disposition = Active
                 | ContainerClosed
                 | Damaged
                 | Destroyed
                 | Dispensed
                 | Disposed
                 | Encoded
                 | Expired
                 | InProgress
                 | InTransit
                 | Inactive
                 | NoPedigreeMatch
                 | NonSellableOther
                 | PartiallyDispensed
                 | Recalled
                 | Reserved
                 | RetailSold
                 | Returned
                 | SellableAccessible
                 | SellableNotAccessible
                 | Stolen
                 | Unknown
                 deriving (Show, Eq, Generic, Read)

makeClassyPrisms ''Disposition

ppDisposition :: Disposition -> String
ppDisposition = revertCamelCase . show

instance URI Disposition where
  uriPrefix _     = "urn:epcglobal:cbv"
  uriQuantifier _ = "disp"
  uriPayload      = ppDisposition

mkDisposition' :: String -> Maybe Disposition
mkDisposition' = mkByName

mkDisposition :: String -> Maybe Disposition
mkDisposition s = let uri = "urn:epcglobal:cbv:disp" in
                         parseURI s uri :: Maybe Disposition

data DWhy = DWhy (Maybe BizStep) (Maybe Disposition)
  deriving (Show, Eq, Generic)

instance HasBizStep DWhy where
  bizStep =
    lens
    (\(DWhy (Just b) _) -> b)
    (\(DWhy _ d) b -> DWhy (Just b) d)

-- FIXME The usage of lens should be reconsidered
-- Nothing is the cause
makeClassy ''DWhy

mkDWhy :: Maybe BizStep -> Maybe Disposition -> Maybe DWhy
mkDWhy step disp = if isNothing step || isNothing disp then Nothing
                     else Just $ DWhy step disp
