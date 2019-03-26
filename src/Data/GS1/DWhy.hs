{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.GS1.DWhy
  (DWhy(..)
  , mkDWhy
  , dispositionValidFor
  )
  where

import           Data.Aeson
import           Data.GS1.EPC
import           Data.GS1.Utils ( optionally )
import           Data.Swagger (ToSchema)
import           GHC.Generics (Generic)

data DWhy = DWhy
            {
              _DWhyBizStep     :: Maybe BizStep
            , _DWhyDisposition :: Maybe Disposition
            }
  deriving (Show, Eq, Generic)

instance ToSchema DWhy

instance FromJSON DWhy where
  parseJSON = withObject "DWhy" $ \o ->
    DWhy <$> o .:? "bizStep"
         <*> o .:? "disposition"

instance ToJSON DWhy where
  toJSON (DWhy a b) = object $ [] <> optionally "bizStep" a
                                  <> optionally "disposition" b
                      
                                  
                             

-- given a disposition, returns the list of valid BizSteps
dispositionValidList :: Disposition -> [BizStep]
dispositionValidList
    Active               = [Commissioning]
dispositionValidList
    ContainerClosed      = [StagingOutbound]
dispositionValidList
    Damaged              = [Accepting, Inspecting, Receiving, Removing, Repairing, Replacing]
dispositionValidList
    Destroyed            = [Destroying]
dispositionValidList
    Dispensed            = [] -- nothing defined - page 25 of spec
dispositionValidList
    Encoded              = [Encoding]
dispositionValidList
    Expired              = [Holding, StagingOutbound, Storing]
dispositionValidList
    InProgress           = [Receiving, Picking, Loading, Accepting, StagingOutbound, Arriving, VoidShipping]
dispositionValidList
    InTransit            = [Shipping, Departing]
dispositionValidList
    Inactive             = [Decommissioning]
dispositionValidList
    NoPedigreeMatch      = [Holding, StagingOutbound, Storing]
dispositionValidList
    NonSellableOther     = [Holding, Inspecting, StagingOutbound, Storing]
dispositionValidList
    PartiallyDispensed   = []
dispositionValidList
    Recalled             = [Holding, StagingOutbound, Storing]
dispositionValidList
    Reserved             = [Reserving]
dispositionValidList
    RetailSold           = [RetailSelling]
dispositionValidList
    Returned             = [Receiving, Holding, Shipping]
dispositionValidList
    SellableAccessible   = [Stocking, Receiving]
dispositionValidList
    SellableNotAccessible= [Receiving, Storing, Loading, Holding, Inspecting]
dispositionValidList
    Stolen               = [] -- nothing defined - page 25 of spec
dispositionValidList
    Unknown              = [] -- nothing defined - page 25 of spec
dispositionValidList
    Disposed             = [] -- nothing defined - page 25 of spec


dispositionValidFor :: BizStep -> Disposition -> Bool
dispositionValidFor bs disp = bs `elem` dispositionValidList disp

mkDWhy :: Either ParseFailure BizStep
       -> Either ParseFailure Disposition
       -> Either ParseFailure DWhy
mkDWhy (Right step)       (Right disp)               = Right $ DWhy (Just step) (Just disp)
mkDWhy (Left (TagNotFound _)) (Left (TagNotFound _)) = Right $ DWhy Nothing Nothing
mkDWhy (Left (TagNotFound _)) (Right disp)           = Right $ DWhy Nothing (Just disp)
mkDWhy (Right step)       (Left (TagNotFound _))     = Right $ DWhy (Just step) Nothing
mkDWhy (Left eBiz)        (Left eDisp)               = Left $ ChildFailure [eBiz, eDisp]
mkDWhy (Left eBiz)        (Right _)                  = Left eBiz
mkDWhy (Right _)          (Left eDisp)               = Left eDisp
