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
import Data.Aeson.Text
import Data.ByteString.Char8 (pack)
import qualified Data.Text.Lazy as TxtL


-- |The What dimension specifies what physical or digital objects
-- participated in the event
data DWhat = -- ObjectDWhat action epcList quantityList
          ObjectDWhat Action [LabelEPC] [QuantityElement]
           -- AggregationDWhat action parentID childEPC childQuantityList
           | AggregationDWhat Action (Maybe ParentID) [LabelEPC] [QuantityElement]
           -- TransactionDWhat action parentID(URI) bizTransactionList epcList quantityList
           | TransactionDWhat Action (Maybe ParentID) [BizTransaction] [LabelEPC] [QuantityElement]
           -- TransformationDWhat transformationID inputEPCList inputQuantityList outputEPCList outputQuantityList
           | TransformationDWhat (Maybe TransformationID) [LabelEPC] [QuantityElement] [LabelEPC] [QuantityElement]
           deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions ''DWhat)
instance ToSchema DWhat

instance ToField DWhat where
  toField = toField . TxtL.toStrict . encodeToLazyText

ppDWhat :: DWhat -> String
ppDWhat (ObjectDWhat a epcs qs) = "OBJECT WHAT\n" ++ show a ++ "\n" ++ show epcs ++ "\n" ++ show qs
ppDWhat (AggregationDWhat a pid epcs qs) = "AGGREGATION WHAT\n" ++ show a ++ "\n" ++ show pid ++ "\n" ++ show epcs ++ "\n" ++ show qs
ppDWhat (TransactionDWhat a s bizT epcs qs) = "TRANSACTION WHAT\n" ++ show a ++ "\n" ++ show s ++ "\n" ++ show bizT ++ "\n" ++ show epcs ++ "\n" ++ show qs
ppDWhat (TransformationDWhat tid iepcs iqs oepcs oqs) = "TRANSFORMATION WHAT\n" ++ show tid ++ "\n" ++ show iepcs ++ "\n" ++ show iqs ++ "\n" ++ show oepcs ++ "\n" ++ show oqs

makeClassy ''DWhat
