{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.GS1.DWhy where

import           GHC.Generics
import           Data.GS1.EPC
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Text
import           Data.Swagger
import           Database.SQLite.Simple.ToField
import qualified Data.Text.Lazy as TxtL

data DWhy = DWhy (Maybe BizStep) (Maybe Disposition)
  deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''DWhy)
instance ToSchema DWhy

instance ToField DWhy where
  toField = toField . TxtL.toStrict . encodeToLazyText

-- given a disposition, returns the list of valid BizSteps
dispositionValidList :: Disposition -> [BizStep] 
dispositionValidList Active =  [Commissioning]
dispositionValidList ContainerClosed =  [StagingOutbound]
dispositionValidList Damaged =
    [Accepting, Inspecting, Receiving, Removing, Repairing, Replacing]
dispositionValidList Destroyed =  [Destroying]
dispositionValidList Dispensed =  [] -- nothing defined - page 25 of spec
dispositionValidList Encoded =  [Encoding] 
dispositionValidList Expired =  [Holding, StagingOutbound, Storing] 
dispositionValidList InProgress = 
    [Receiving, Picking, Loading, Accepting, StagingOutbound] ++
    [Arriving, VoidShipping] 
dispositionValidList InTransit =  [Shipping, Departing] 
dispositionValidList Inactive   =  [Decommissioning] 
dispositionValidList NoPedigreeMatch   =  [Holding, StagingOutbound, Storing] 
dispositionValidList NonSellableOther = 
    [Holding, Inspecting, StagingOutbound, Storing] 
dispositionValidList PartiallyDispensed =  []
dispositionValidList Recalled =  [Holding, StagingOutbound, Storing] 
dispositionValidList Reserved =  [Reserving] 
dispositionValidList RetailSold =  [RetailSelling] 
dispositionValidList Returned =  [Receiving, Holding, Shipping] 
dispositionValidList SellableAccessible =  [Stocking, Receiving] 
dispositionValidList SellableNotAccessible =
    [Receiving, Storing, Loading, Holding, Inspecting] 
dispositionValidList Stolen =  [] -- nothing defined - page 25 of spec
dispositionValidList Unknown =  [] -- nothing defined - page 25 of spec


dispositionValidFor :: BizStep -> Disposition -> Bool
dispositionValidFor bs disp = bs `elem` dispositionValidList disp

mkDWhy :: Either ParseFailure BizStep -> Either ParseFailure Disposition
          -> Either ParseFailure DWhy
mkDWhy (Right step) (Right disp) = Right $ DWhy (Just step) (Just disp)
mkDWhy (Left TagNotFound) (Left TagNotFound) = Right $ DWhy Nothing Nothing
mkDWhy (Left TagNotFound) (Right disp) = Right $ DWhy Nothing (Just disp)
mkDWhy (Right step) (Left TagNotFound) = Right $ DWhy (Just step) Nothing
mkDWhy (Left eBiz) (Left eDisp) = Left $ ChildFailure [eBiz, eDisp]
mkDWhy (Left eBiz) (Right _) = Left $ ChildFailure [eBiz]
mkDWhy (Right _) (Left eDisp) = Left $ ChildFailure [eDisp]
