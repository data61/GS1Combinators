{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | module for the What Dimension
-- contains labelepc and printer and parser for it, and DWhat type

module Data.GS1.DWhat where

import           GHC.Generics

import           Data.Semigroup

import           Data.Aeson
import           Data.Aeson.TH

import           Data.Swagger
import qualified Data.Text      as T

import           Data.GS1.EPC

data LabelEPC = CL 
                {
                  _clClassLabelEpc :: ClassLabelEPC
                , _clQuantity      :: Maybe Quantity
                }
              | IL
                {
                  _ilInstanceLabelEpc :: InstanceLabelEPC
                }
              deriving (Show, Read, Eq, Generic)

$(deriveJSON defaultOptions ''LabelEPC)
instance ToSchema LabelEPC

type ParentLabel  = InstanceLabelEPC
type InputEPC  = LabelEPC
type OutputEPC = LabelEPC

-- | Parses an EPC URN into a LabelEPC.
-- As no quantity information is provided in a URN,
-- Maybe Quantity is provided so ClassLabel EPCs can be instantiated with it.
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

-- |The What dimension specifies what physical or digital objects
-- participated in the event
data DWhat = -- ObjectDWhat action epcList
            ObjectDWhat
            { 
              _objAction  :: Action
            , _objEpcList :: [LabelEPC]
            }
          -- AggregationDWhat action parentLabel childEPC
          | AggregationDWhat
            {
              _aggAction       :: Action
            , _aggParentLabel  :: Maybe ParentLabel
            , _aggChildEpcList :: [LabelEPC]
            }
          -- TransactionDWhat action parentLabel(URI) bizTransactionList epcList
          -- EPCIS-Standard-1.2-r-2016-09-29.pdf Page 56
          | TransactionDWhat
            {
              _transactionAction             :: Action 
            , _transactionParentLabel        :: Maybe ParentLabel
            , _transactionBizTransactionList :: [BizTransaction]
            , _transactionEpcList            :: [LabelEPC]
            }
          -- TransformationDWhat transformationID inputEPCList outputEPCList
          | TransformationDWhat
            {
              _TransformationId            :: Maybe TransformationID
            , _TransformationInputEpcList  :: [InputEPC]
            , _TransformationOutputEpcList :: [OutputEPC]
            }
          deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions ''DWhat)
instance ToSchema DWhat

ppDWhat :: DWhat -> String
ppDWhat (ObjectDWhat a epcs) =
  "OBJECT WHAT\n" ++ show a ++ "\n" ++ show epcs ++ "\n"
ppDWhat (AggregationDWhat a pid epcs ) =
  "AGGREGATION WHAT\n" ++ show a ++ "\n" ++ show pid ++ "\n" ++ show epcs ++ "\n"
ppDWhat (TransactionDWhat a s bizT epcs ) =
  "TRANSACTION WHAT\n" ++ show a ++ "\n" ++ show s ++ "\n" ++ show bizT ++ "\n" ++ show epcs ++ "\n"
ppDWhat (TransformationDWhat tid inputEpcs outputEpcs ) =
  "TRANSFORMATION WHAT\n" ++ show tid ++ "\n" ++ show inputEpcs ++ "\n" ++ show outputEpcs ++ "\n"
