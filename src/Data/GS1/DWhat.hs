{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.GS1.DWhat where

import           Control.Lens
import           Data.Char
import           GHC.Generics

import           Data.GS1.EPC
import           Data.GS1.Object
import           Data.GS1.URI
import           Data.GS1.Utils

import           Data.Aeson
import           Data.Aeson.TH

import           Data.Swagger
{-
  Example:

  <bizTransactionList>
    <bizTransaction type="urn:epcglobal:cbv:btt:po">
      http://transaction.acme.com/po/12345678
    </bizTransaction>
  </bizTransactionList>
-}


type BizTransactionID = String

data BizTransactionType = Bol       -- Bill of Lading
                        | Desadv    -- Dispatch Advice
                        | Inv       -- Invoice
                        | Pedigree  -- Pedigree
                        | Po        -- Purchase Order
                        | Poc       -- Purchase Order Confirmation
                        | Prodorder -- Production Order
                        | Recadv    -- Receiving Advice
                        | Rma       -- Return Mechandise Authorisation
                        deriving (Show, Eq, Generic, Read)
$(deriveJSON defaultOptions ''BizTransactionType)
instance ToSchema BizTransactionType

ppBizTransactionType :: BizTransactionType -> String
ppBizTransactionType = revertCamelCase . show

instance URI BizTransactionType where
  uriPrefix _     = "urn:epcglobal:cbv"
  uriQuantifier _ = "btt"
  uriPayload      = ppBizTransactionType

mkBizTransactionType :: String -> Maybe BizTransactionType
mkBizTransactionType = mkByName

parseBizTransactionType :: String -> Maybe BizTransactionType
parseBizTransactionType s = let uri = "urn:epcglobal:cbv:btt" in
                                parseURI s uri :: Maybe BizTransactionType

-- |BizTransaction CBV Section 7.3 and Section 8.5
data BizTransaction = BizTransaction
  {
    _btid :: BizTransactionID
  , _bt   :: BizTransactionType
  }
  deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''BizTransaction)
instance ToSchema BizTransaction

makeClassy ''BizTransaction

-- | TransactionType, TransactionID
mkBizTransaction :: String -> String -> Maybe BizTransaction
mkBizTransaction t i = let bt' = parseBizTransactionType t in
                           case bt' of
                             Just t'  -> Just BizTransaction{_btid = i, _bt = t'}
                             _       -> Nothing

type BizTransactionList = [BizTransaction]

-- | TransformationID
type TransformationID = String

-- | ParentID
type ParentID = String

data Action = Add
            | Observe
            | Delete
            deriving (Show, Eq, Generic, Read)
$(deriveJSON defaultOptions ''Action)
instance ToSchema Action

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
           | TransactionDWhat Action (Maybe ParentID) [BizTransaction] [EPC] [QuantityElement]
           -- TransformationDWhat transformationID inputEPCList inputQuantityList outputEPCList outputQuantityList
           | TransformationDWhat (Maybe TransformationID) [EPC] [QuantityElement] [EPC] [QuantityElement]
           deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions ''DWhat)
instance ToSchema DWhat

ppDWhat :: DWhat -> String
ppDWhat (ObjectDWhat a epcs qs) = "OBJECT WHAT\n" ++ show a ++ "\n" ++ show epcs ++ "\n" ++ show qs
ppDWhat (AggregationDWhat a pid epcs qs) = "AGGREGATION WHAT\n" ++ show a ++ "\n" ++ show pid ++ "\n" ++ show epcs ++ "\n" ++ show qs
ppDWhat (QuantityDWhat c i) = "QUANTITY WHAT\n" ++ show c ++ "\n" ++ show i
ppDWhat (TransactionDWhat a s bizT epcs qs) = "TRANSACTION WHAT\n" ++ show a ++ "\n" ++ show s ++ "\n" ++ show bizT ++ "\n" ++ show epcs ++ "\n" ++ show qs
ppDWhat (TransformationDWhat tid iepcs iqs oepcs oqs) = "TRANSFORMATION WHAT\n" ++ show tid ++ "\n" ++ show iepcs ++ "\n" ++ show iqs ++ "\n" ++ show oepcs ++ "\n" ++ show oqs

makeClassy ''DWhat
