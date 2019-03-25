-- {-# LANGUAGE DeriveAnyClass             #-}
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


import           Data.Aeson
import           Data.Aeson.Types ( Parser, Pair )
import           Data.Swagger
import qualified Data.Text    as T
import           GHC.Generics

import           Data.GS1.EventType ( EventType(..), withEvent )
import           Data.GS1.EPC
import           Data.GS1.Utils ( merge )

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

instance FromJSON LabelEPC where
  parseJSON x@(String _) = fmap IL (parseJSON x)
  
  parseJSON x = flip (withObject "LabelEPC") x $ \o ->
    CL <$> o .: "epcClass"
       <*> parseQuantityJSON o
    where
      parseQuantityJSON :: Object -> Parser (Maybe Quantity)
      parseQuantityJSON o = do
        q :: Maybe Value <- o .:? "quantity"
        maybe (pure Nothing) (const $ parseJSON (Object o)) q

instance ToJSON LabelEPC where
  toJSON (IL x) = toJSON x
  toJSON (CL a b) =
    let o = object $ [ "epcClass" .= a ] in
      maybe o (merge o . toJSON) b

-- | Utlity function to extract the GS1CompanyPrefix of a Label
getCompanyPrefix :: LabelEPC -> GS1CompanyPrefix
getCompanyPrefix (IL (GIAI pfx _))       = pfx
getCompanyPrefix (IL (SSCC pfx _))       = pfx
getCompanyPrefix (IL (SGTIN pfx _ _ _))  = pfx
getCompanyPrefix (IL (GRAI pfx _ _))     = pfx
getCompanyPrefix (CL (LGTIN pfx _ _) _)  = pfx
getCompanyPrefix (CL (CSGTIN pfx _ _) _) = pfx

newtype ParentLabel  = ParentLabel {unParentLabel :: InstanceLabelEPC}
  deriving (Show, Read, Eq, Generic, URI, FromJSON, ToJSON)

newtype InputEPC     = InputEPC {unInputEPC :: LabelEPC}
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

newtype OutputEPC    = OutputEPC {unOutputEPC :: LabelEPC}
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

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
  parseJSON = withObject "ObjectDWhat" $ \o ->
    ObjectDWhat <$> o .: "action"
                <*> ( mappend <$> o .:? "epcList" .!= []
                              <*> o .:? "quantityList" .!= []
                    )
                
instance ToJSON ObjectDWhat where
  toJSON (ObjectDWhat a b) = object $ [ "action" .= a ]
                                   <> ("epcList" `ifNotEmpty` instanceLabels b)
                                   <> ("quantityList" `ifNotEmpty` classLabels b)
                                   

ifNotEmpty :: ToJSON a => T.Text -> [a] -> [Pair]
ifNotEmpty _ [] = []
ifNotEmpty k xs = [ k .= xs ]

isInstanceLabel :: LabelEPC -> Bool
isInstanceLabel (IL _) = True
isInstanceLabel _ = False

instanceLabels :: [LabelEPC] -> [LabelEPC]
instanceLabels = filter isInstanceLabel

classLabels :: [LabelEPC] -> [LabelEPC]
classLabels = filter (not . isInstanceLabel)

-- AggregationDWhat action parentLabel childEPC
data AggregationDWhat =
  AggregationDWhat
  {
    _aggAction       :: Action
  , _aggParentLabel  :: Maybe ParentLabel
  , _aggChildEpcList :: [LabelEPC]
  } deriving (Show, Eq, Generic)

instance FromJSON AggregationDWhat where
  parseJSON = withObject "AggregationDWhat" $ \o ->
    AggregationDWhat <$> o .: "action"
                     <*> o .:? "parentID"
                     <*> (mappend <$> o .:? "childEPCs" .!= []
                                  <*> o .:? "childQuantityList" .!= []
                         )
                     
instance ToJSON AggregationDWhat where
  toJSON (AggregationDWhat a b c) =
    object $ [ "action" .= a
             , "parentID" .= b
             ] <> ("childEPCs" `ifNotEmpty` instanceLabels c)
               <> ("childQuantityList" `ifNotEmpty` classLabels c)
           

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
  parseJSON = withObject "TransactionDWhat" $ \o ->
    TransactionDWhat <$> o .: "action"
                     <*> o .:? "parentID"
                     <*> o .: "bizTransactionList"
                     <*> o .: "epcList"
  
instance ToJSON TransactionDWhat where
  toJSON (TransactionDWhat a b c d) =
    object [ "action" .= a
           , "parentID" .= b
           , "bizTransactionList" .= c
           , "epcList" .= d
           ]

-- TransformationDWhat transformationId inputEPCList outputEPCList
data TransformationDWhat =
  TransformationDWhat
  {
    _transformationId            :: Maybe TransformationId
  , _transformationInputEpcList  :: [InputEPC]
  , _transformationOutputEpcList :: [OutputEPC]
  } deriving (Show, Eq, Generic)

instance FromJSON TransformationDWhat where
  parseJSON = withObject "TransformationDWhat" $ \o ->
    TransformationDWhat <$> o .:? "transformationID"
                        <*> o .: "inputEPCList"
                        <*> o .: "outputEPCList"
                        
instance ToJSON TransformationDWhat where
  toJSON (TransformationDWhat a b c) =
    object [ "transformationID" .= a
           , "inputEPCList" .= b
           , "outputEPCList" .= c
           ]

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
