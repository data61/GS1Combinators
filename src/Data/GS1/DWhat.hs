{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}


module Data.GS1.DWhat
  (LabelEPC(..)
  , ParentLabel(..)
  , InputEPC(..)
  , OutputEPC (..)
  , ObjectDWhat(..)
  , AggregationDWhat(..)
  , TransactionDWhat(..)
  , TransformationDWhat(..)
  , DWhat(..)
  , readLabelEPC
  , urn2LabelEPC
  )
  where

import           GHC.Generics

import           Data.Semigroup

import           Data.Aeson
import           Data.Aeson.TH

import           Data.Swagger
import qualified Data.Text      as T

import           Data.GS1.EPC

data LabelEPC
  = CL
    { _clClassLabelEpc :: ClassLabelEPC
    , _clQuantity      :: Maybe Quantity
    }
  | IL
    { _ilInstanceLabelEpc :: InstanceLabelEPC
    }
  deriving (Show, Read, Eq, Generic)

$(deriveJSON defaultOptions ''LabelEPC)
instance ToSchema LabelEPC

newtype ParentLabel  = ParentLabel {unParentLabel :: InstanceLabelEPC}
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, URI)
newtype InputEPC     = InputEPC {unInputEPC :: LabelEPC}
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
newtype OutputEPC    = OutputEPC {unOutputEPC :: LabelEPC}
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

instance ToSchema ParentLabel
instance ToSchema InputEPC
instance ToSchema OutputEPC


-- | Parses an EPC URN into a LabelEPC.
-- As no quantity information is provided in a URN,
-- Maybe Quantity is provided so ClassLabel EPCs can be instantiated with it.

-- TODO: This feels wrong, and should probably use Alternative's <|>
readLabelEPC :: Maybe Quantity -> T.Text -> Either ParseFailure LabelEPC
readLabelEPC mQt epcStr =
  fmap (`CL` mQt) (readURIClassLabelEPC epcTokens)
    <>
      fmap IL (readURIInstanceLabelEPC epcTokens)
  where
    epcTokens = T.splitOn ":" epcStr

-- | Given an EPC URN, return a LabelEPC.
-- As no quantity information is provided in the URN,
-- the quantity field in ClassLabelEPC is set to Nothing.
-- Use this when you do not need Quantity information
urn2LabelEPC :: T.Text -> Either ParseFailure LabelEPC
urn2LabelEPC = readLabelEPC Nothing

-- ObjectDWhat action epcList
data ObjectDWhat =
  ObjectDWhat
  {
    _objAction  :: Action
  , _objEpcList :: [LabelEPC]
  } deriving (Show, Eq, Generic)

-- AggregationDWhat action parentLabel childEPC
data AggregationDWhat =
  AggregationDWhat
  {
    _aggAction       :: Action
  , _aggParentLabel  :: Maybe ParentLabel
  , _aggChildEpcList :: [LabelEPC]
  } deriving (Show, Eq, Generic)

-- TransactionDWhat action parentLabel(URI) bizTransactionList epcList
-- EPCIS-Standard-1.2-r-2016-09-29.pdf Page 56
data TransactionDWhat =
  TransactionDWhat
  {
    _transactionAction             :: Action
  , _transactionParentLabel        :: Maybe ParentLabel
  , _transactionBizTransactionList :: [BizTransaction]
  , _transactionEpcList            :: [LabelEPC]
  } deriving (Show, Eq, Generic)

-- TransformationDWhat transformationId inputEPCList outputEPCList
data TransformationDWhat =
  TransformationDWhat
  {
    _transformationId            :: Maybe TransformationId
  , _transformationInputEpcList  :: [InputEPC]
  , _transformationOutputEpcList :: [OutputEPC]
  } deriving (Show, Eq, Generic)

instance FromJSON ObjectDWhat
instance FromJSON AggregationDWhat
instance FromJSON TransactionDWhat
instance FromJSON TransformationDWhat

instance ToJSON ObjectDWhat
instance ToJSON AggregationDWhat
instance ToJSON TransactionDWhat
instance ToJSON TransformationDWhat

instance ToSchema ObjectDWhat
instance ToSchema AggregationDWhat
instance ToSchema TransactionDWhat
instance ToSchema TransformationDWhat


-- |The What dimension specifies what physical or digital objects
-- participated in the event
data DWhat =
    ObjWhat ObjectDWhat
  | AggWhat AggregationDWhat
  | TransactWhat TransactionDWhat
  | TransformWhat TransformationDWhat
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions ''DWhat)
instance ToSchema DWhat

