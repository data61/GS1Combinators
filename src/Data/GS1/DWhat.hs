{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.GS1.DWhat where

import           Control.Lens
import           Data.Char
import           GHC.Generics

import           Data.GS1.EPC
import           Data.GS1.Object
import           Data.GS1.Utils

import           Data.Aeson
import           Data.Aeson.TH

import           Data.Swagger
import           Database.SQLite.Simple.ToField
import           Data.Aeson.Text
import           Data.ByteString.Char8 (pack)
import qualified Data.Text.Lazy as TxtL

data LabelEPC = CL ClassLabelEPC (Maybe Quantity) | IL InstanceLabelEPC
                deriving (Show, Read, Eq, Generic)

$(deriveJSON defaultOptions ''LabelEPC)
instance ToSchema LabelEPC

-- | ParentID
type ParentID = LabelEPC

-- |The What dimension specifies what physical or digital objects
-- participated in the event
data DWhat = -- ObjectDWhat action epcList quantityList
             ObjectDWhat Action [LabelEPC] [Quantity]
           -- AggregationDWhat action parentID childEPC
           | AggregationDWhat Action (Maybe ParentID) [LabelEPC] -- should this have [quantity]? @sa
           -- TransactionDWhat action parentID(URI) bizTransactionList epcList
           | TransactionDWhat Action (Maybe ParentID) [BizTransaction] [LabelEPC]
           -- TransformationDWhat transformationID inputEPCList outputEPCList
           | TransformationDWhat (Maybe TransformationID) [LabelEPC]  [LabelEPC]
           deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions ''DWhat)
instance ToSchema DWhat

instance ToField DWhat where
  toField = toField . TxtL.toStrict . encodeToLazyText

ppDWhat :: DWhat -> String
ppDWhat (ObjectDWhat a epcs qs) = "OBJECT WHAT\n" ++ show a ++ "\n" ++ show epcs ++ "\n" ++ show qs ++ "\n"
ppDWhat (AggregationDWhat a pid epcs ) = "AGGREGATION WHAT\n" ++ show a ++ "\n" ++ show pid ++ "\n" ++ show epcs ++ "\n"
ppDWhat (TransactionDWhat a s bizT epcs ) = "TRANSACTION WHAT\n" ++ show a ++ "\n" ++ show s ++ "\n" ++ show bizT ++ "\n" ++ show epcs ++ "\n"
ppDWhat (TransformationDWhat tid inputEpcs outputEpcs ) = "TRANSFORMATION WHAT\n" ++ show tid ++ "\n" ++ show inputEpcs ++ "\n" ++ show outputEpcs ++ "\n"

makeClassy ''DWhat
