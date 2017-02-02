{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.GS1.Disposition where

import           Control.Lens.TH
import           Data.GS1.BizStep
import           Data.GS1.URI
import           Data.GS1.Utils
import           GHC.Generics

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

-- FIXME: it could be just an example page 24/64 CBV
-- Valid Dispositions, defined in section CBV 7.2
dispositionValidList :: Disposition -> [BizStep]
dispositionValidList Active                = [Commissioning]
dispositionValidList ContainerClosed       = [StagingOutbound]
dispositionValidList Damaged               = [Accepting, Inspecting, Receiving, Removing, Repairing, Replacing]
dispositionValidList Destroyed             = [Destroying]
dispositionValidList Dispensed             = [] -- nothing defined - page 25 of spec
dispositionValidList Disposed              = [] --FIXME
dispositionValidList Encoded               = [Encoding]
dispositionValidList Expired               = [Holding, StagingOutbound, Storing]
dispositionValidList InProgress            = [Receiving, Picking, Loading, Accepting, StagingOutbound, Arriving, VoidShipping]
dispositionValidList InTransit             = [Shipping, Departing]
dispositionValidList Inactive              = [Decommissioning]
dispositionValidList NoPedigreeMatch       = [Holding, StagingOutbound, Storing]
dispositionValidList NonSellableOther      = [Holding, Inspecting, StagingOutbound, Storing]
dispositionValidList PartiallyDispensed    = []  -- nothing defined - page 25 of spec
dispositionValidList Recalled              = [Holding, StagingOutbound, Storing]
dispositionValidList Reserved              = [Reserving]
dispositionValidList RetailSold            = [RetailSelling]
dispositionValidList Returned              = [Receiving, Holding, Shipping]
dispositionValidList SellableAccessible    = [Stocking, Receiving]
dispositionValidList SellableNotAccessible = [Receiving, Storing, Loading, Holding, Inspecting]
dispositionValidList Stolen                = [] -- nothing defined - page 25 of spec
dispositionValidList Unknown               = [] -- nothing defined - page 25 of spec

dispositionValidFor :: BizStep -> Disposition -> Bool
dispositionValidFor b d = b `elem` dispositionValidList d
