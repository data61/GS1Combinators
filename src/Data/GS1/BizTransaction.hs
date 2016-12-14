{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.GS1.BizTransaction where

import           Control.Lens.TH
import           Data.GS1.URI
import           Data.GS1.Utils
import           GHC.Generics

-- BTI stands for Business Transaction Identifier
-- BTT stands for Business Transaction Type
-- GDTI stands for Global Document Type Identifier
-- GSRN stands for Global Service Relation Number
data BizTransactionID = BTIGDTI String
                      | BTIGSRN String
                      | BTIGLN String
                      deriving (Show, Eq, Generic)

data BizTransactionType = Bol       -- Bill of Lading
                        | Desadv    -- Dispatch Advice
                        | Inv       -- Invoice
                        | Pedigree  -- Pedigree
                        | Po        -- Purchase Order
                        | Poc       -- Purchase Order Confirmation
                        | Prodorder -- Production Order
                        | Recadv    -- Receiving Advice
                        | Rma       -- Return Mechandise Authorisation
                        deriving (Show, Eq, Generic)

ppBizTransactionType :: BizTransactionType -> String
ppBizTransactionType = revertCamelCase . show

instance URI BizTransactionType where
  uriPrefix _     = "urn:epcglobal:cbv"
  uriQuantifier _ = "btt"
  uriPayload      = ppBizTransactionType

-- |BizTransaction CBV Section 7.3 and Section 8.5
-- MAY contain one or more BizTransactionType means [0..*]
data BizTransaction = BizTransaction
  {
    _btid     :: BizTransactionID
  , _typeList :: [BizTransactionType]
  }
  deriving (Show, Eq, Generic)

makeClassy ''BizTransaction

instance URI BizTransactionID where
  uriPrefix a = case a of
                  BTIGDTI _ -> "urn:epc:id"
                  BTIGSRN _ -> "urn:epc:id"
                  BTIGLN _  -> "urn:epcglobal:cbv:bt"
  uriQuantifier a = case a of
                      BTIGDTI _ -> "gdti"
                      BTIGSRN _ -> "gsrn"
                      BTIGLN  _ -> "gln"
  uriPayload a = case a of
                   BTIGDTI b -> b
                   BTIGSRN b -> b
                   BTIGLN b  -> b
