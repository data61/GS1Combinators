{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

{-
  Example:

  <bizTransactionList>
    <bizTransaction type="urn:epcglobal:cbv:btt:po">
      http://transaction.acme.com/po/12345678
    </bizTransaction>
  </bizTransactionList>
 -}

module Data.GS1.BizTransaction where

import           Control.Lens.TH
import           Data.GS1.URI
import           Data.GS1.Utils
import           GHC.Generics

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

makeClassy ''BizTransaction

type BizTransactionList = [BizTransaction]
