{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}


module Data.GS1.DWhat where

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

newtype ParentLabel  = ParentLabel InstanceLabelEPC deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, URI)
newtype InputEPC     = InputEPC LabelEPC            deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
newtype OutputEPC    = OutputEPC LabelEPC           deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
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

-- |The What dimension specifies what physical or digital objects
-- participated in the event

-- TODO: Split this out into separate types for each constructor and then use
-- this as a sum type which just contains one type each - in supplyChainServer
-- several of these types are replicated because we're only expecting a single
-- option to be sent in the body - see Model.hs
data DWhat
    -- | ObjectDWhat action epcList
    = ObjectDWhat
    { _objAction  :: Action
    , _objEpcList :: [LabelEPC]
    }
  -- | AggregationDWhat action parentLabel childEPC
  | AggregationDWhat
    { _aggAction       :: Action
    , _aggParentLabel  :: Maybe ParentLabel
    , _aggChildEpcList :: [LabelEPC]
    }
  -- | TransactionDWhat action parentLabel(URI) bizTransactionList epcList
  -- EPCIS-Standard-1.2-r-2016-09-29.pdf Page 56
  | TransactionDWhat
    { _transactionAction             :: Action
    , _transactionParentLabel        :: Maybe ParentLabel
    , _transactionBizTransactionList :: [BizTransaction]
    , _transactionEpcList            :: [LabelEPC]
    }
  -- | TransformationDWhat transformationID inputEPCList outputEPCList
  | TransformationDWhat
    { _TransformationId            :: Maybe TransformationID
    , _TransformationInputEpcList  :: [InputEPC]
    , _TransformationOutputEpcList :: [OutputEPC]
    }
    deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions ''DWhat)
instance ToSchema DWhat


-- TODO: Consider using a proper pretty printer
ppDWhat :: DWhat -> String
ppDWhat (ObjectDWhat a epcs) =
  "OBJECT WHAT\n" ++ show a ++ "\n" ++ show epcs ++ "\n"
ppDWhat (AggregationDWhat a pid epcs ) =
  "AGGREGATION WHAT\n" ++ show a ++ "\n" ++ show pid ++ "\n" ++ show epcs ++ "\n"
ppDWhat (TransactionDWhat a s bizT epcs ) =
  "TRANSACTION WHAT\n" ++ show a ++ "\n" ++ show s ++ "\n" ++ show bizT ++ "\n" ++ show epcs ++ "\n"
ppDWhat (TransformationDWhat tid inputEpcs outputEpcs ) =
  "TRANSFORMATION WHAT\n" ++ show tid ++ "\n" ++ show inputEpcs ++ "\n" ++ show outputEpcs ++ "\n"
