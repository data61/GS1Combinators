{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.GS1.DWhat where

import           Control.Lens
import           Data.Char
import           GHC.Generics

import           Data.GS1.BizTransaction
import           Data.GS1.EPC
import           Data.GS1.EventID
import           Data.GS1.Object
import           Data.GS1.Utils

-- | TransformationID
type TransformationID = String

-- | ParentID
type ParentID = String

data Action = Add
            | Observe
            | Delete
            deriving (Show, Eq, Generic, Read)

mkAction :: String -> Maybe Action
mkAction s = mkByName . camelCase $ toLower <$> s

-- |The What dimension specifies what physical or digital objects
-- participated in the event
data DWhat = -- ObjectDWhat action epcList quantityList
           ObjectDWhat Action [EPC] [QuantityElement]
           -- AggregationDWhat action parentID childEPC childQuantityList
           | AggregationDWhat Action (Maybe ParentID) [EPC] [QuantityElement]
           -- QuantityDWhat epcClass quantity
           | QuantityDWhat EPCClass Integer
           -- TransactionDWhat action parentID(URI) bizTransactionList epcList quantityList
           | TransactionDWhat Action String [BizTransaction] [EPC] [QuantityElement]
           -- TransformationDWhat transformationID inputEPCList inputQuantityList outputEPCList outputQuantityList
           | TransformationDWhat (Maybe TransformationID) [EPC] [QuantityElement] [EPC] [QuantityElement]
           deriving (Show, Eq, Generic)

ppDWhat :: DWhat -> String
ppDWhat (ObjectDWhat a epcs qs) = "OBJECT WHAT\n" ++ show a ++ "\n" ++ show epcs ++ "\n" ++ show qs
ppDWhat (AggregationDWhat a pid epcs qs) = "AGGREGATION WHAT\n" ++ show a ++ "\n" ++ show pid ++ "\n" ++ show epcs ++ "\n" ++ show qs
ppDWhat (QuantityDWhat c i) = "QUANTITY WHAT\n" ++ show c ++ "\n" ++ show i
ppDWhat (TransactionDWhat a s bizT epcs qs) = "TRANSACTION WHAT\n" ++ show a ++ "\n" ++ show s ++ "\n" ++ show bizT ++ "\n" ++ show epcs ++ "\n" ++ show qs
ppDWhat (TransformationDWhat tid iepcs iqs oepcs oqs) = "TRANSFORMATION WHAT\n" ++ show tid ++ "\n" ++ show iepcs ++ "\n" ++ show iqs ++ "\n" ++ show oepcs ++ "\n" ++ show oqs

makeClassy ''DWhat
