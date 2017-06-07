{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.GS1.DWhy where

import           Control.Lens
import           Data.Maybe
import           GHC.Generics

import           Data.GS1.URI
import           Data.GS1.Utils
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Text
import           Data.Swagger
import           Database.SQLite.Simple.ToField
import qualified Data.Text.Lazy as TxtL

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
$(deriveJSON defaultOptions ''BizStep)
instance ToSchema BizStep

makeClassy ''BizStep
-- XXX - you might also want makeClassyPrisms for BizStep (as well as, or instead of, makeClassy)

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
$(deriveJSON defaultOptions ''DispositionError)
instance ToSchema DispositionError

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
$(deriveJSON defaultOptions ''Disposition)
instance ToSchema Disposition

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
$(deriveJSON defaultOptions ''DWhy)
instance ToSchema DWhy

instance ToField DWhy where
  toField = toField . TxtL.toStrict . encodeToLazyText
{-
XXX - FIXME
this is not a lawful lens
it is a Traversal though
you might also want makeClassyPrisms for BizStep (as well as, or instead of, makeClassy)
-}
instance HasBizStep DWhy where
  bizStep =
    lens
    (\(DWhy (Just b) _) -> b)
    (\(DWhy _ d) b -> DWhy (Just b) d)

-- FIXME The usage of lens should be reconsidered
-- Nothing is the cause
makeClassy ''DWhy

-- XXX mkDWhy can be rewritten as:
--mkDWhy = liftA2 DWhy
mkDWhy :: Maybe BizStep -> Maybe Disposition -> Maybe DWhy
mkDWhy step disp = if isNothing step || isNothing disp then Nothing
                     else Just $ DWhy step disp
