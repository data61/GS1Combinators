{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.GS1.DWhat where

import           Control.Lens
import           Data.GS1.BizTransaction
import           Data.GS1.EPC
import           Data.GS1.EventID
import           Data.GS1.Object
import           GHC.Generics

type TransformationID = String

data Action = Add
            | Observe
            | Delete
            deriving (Show, Eq, Generic)

-- |The What dimension specifies what physical or digital objects
-- participated in the event
data DWhat = -- ObjectDWhat action epcList quantityList
           ObjectDWhat Action [EPC] [QuantityElement]
           -- AggregationDWhat parentID childEPC childQuantityList
           | AggregationDWhat (Maybe EventID) [EPC] [QuantityElement]
           -- QuantityDWhat epcClass quantity
           | QuantityDWhat EPCClass Integer
           -- TransactionDWhat action parentID(URI) bizTransactionList epcList quantityList
           | TransactionDWhat Action String [BizTransaction] [EPC] [QuantityElement]
           -- TransformationDWhat transformationID inputEPCList inputQuantityList outputEPCList outputQuantityList
           | TransformationDWhat (Maybe TransformationID) [EPC] [QuantityElement] [EPC] [QuantityElement]
           deriving (Show, Eq, Generic)

makeClassy ''DWhat
