{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

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
  , getCompanyPrefix
  )
  where

import           GHC.Generics

import           Data.Aeson

import           Data.Swagger
import qualified Data.Text    as T

import           Data.GS1.EventType ( EventType(..), withEvent )
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

instance ToSchema LabelEPC

-- instance FromJSON LabelEPC where
--   parseJSON = withObject "LabelEPC" $ \o -> do
--     (o .:? "quantityElement") >>= \case
--     -- case (r :: Maybe ClassLabelEPC) of
--       Nothing -> error "lol"
--       Just (e :: ClassLabelEPC)  -> error $ show e

-- instance ToJSON LabelEPC where
--   toJSON _l = error "lol"


-- | Utlity function to extract the GS1CompanyPrefix of a Label
getCompanyPrefix :: LabelEPC -> GS1CompanyPrefix
getCompanyPrefix (IL (GIAI pfx _))       = pfx
getCompanyPrefix (IL (SSCC pfx _))       = pfx
getCompanyPrefix (IL (SGTIN pfx _ _ _))  = pfx
getCompanyPrefix (IL (GRAI pfx _ _))     = pfx
getCompanyPrefix (CL (LGTIN pfx _ _) _)  = pfx
getCompanyPrefix (CL (CSGTIN pfx _ _) _) = pfx

newtype ParentLabel  = ParentLabel {unParentLabel :: InstanceLabelEPC}
  deriving (Show, Read, Eq, Generic, URI)
newtype InputEPC     = InputEPC {unInputEPC :: LabelEPC}
  deriving (Show, Read, Eq, Generic)
newtype OutputEPC    = OutputEPC {unOutputEPC :: LabelEPC}
  deriving (Show, Read, Eq, Generic)

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

instance FromJSON ObjectDWhat where
  parseJSON = undefined
instance ToJSON ObjectDWhat where
  toJSON = undefined

-- AggregationDWhat action parentLabel childEPC
data AggregationDWhat =
  AggregationDWhat
  {
    _aggAction       :: Action
  , _aggParentLabel  :: Maybe ParentLabel
  , _aggChildEpcList :: [LabelEPC]
  } deriving (Show, Eq, Generic)

instance FromJSON AggregationDWhat where
  parseJSON = undefined
instance ToJSON AggregationDWhat where
  toJSON = undefined

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

instance FromJSON TransactionDWhat where
  parseJSON = undefined
instance ToJSON TransactionDWhat where
  toJSON = undefined

-- TransformationDWhat transformationId inputEPCList outputEPCList
data TransformationDWhat =
  TransformationDWhat
  {
    _transformationId            :: Maybe TransformationId
  , _transformationInputEpcList  :: [InputEPC]
  , _transformationOutputEpcList :: [OutputEPC]
  } deriving (Show, Eq, Generic)

instance FromJSON TransformationDWhat where
  parseJSON = undefined
instance ToJSON TransformationDWhat where
  toJSON = undefined

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

instance ToSchema DWhat

instance FromJSON DWhat where
  parseJSON = withEvent $ \o -> \case
    ObjectEventT -> ObjWhat <$> parseJSON (Object o)
    AggregationEventT -> AggWhat <$> parseJSON (Object o)
    TransactionEventT -> TransactWhat <$> parseJSON (Object o)
    TransformationEventT -> TransformWhat <$> parseJSON (Object o)

instance ToJSON DWhat where
  toJSON (ObjWhat o) = toJSON o
  toJSON (AggWhat o) = toJSON o
  toJSON (TransactWhat o) = toJSON o
  toJSON (TransformWhat o) = toJSON o
