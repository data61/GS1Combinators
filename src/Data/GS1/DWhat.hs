{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.GS1.DWhat where

import           Control.Lens
-- import           Data.Char
import           GHC.Generics

import           Data.List.Split

import           Data.GS1.EPC
-- why can't GHCi find these modules?
import           Data.Aeson
import           Data.Aeson.TH

import           Data.Swagger
import           Database.SQLite.Simple.ToField
import           Data.Aeson.Text
import           Data.ByteString.Char8 (pack)
import qualified Data.Text.Lazy as TxtL

data LabelEPC = CL ClassLabelEPC (Maybe Quantity) | IL InstanceLabelEPC
                deriving (Show, Read, Eq, Generic)

$(deriveJSON defaultOptions ''LabelEPC)
instance ToSchema LabelEPC

instance ToField LabelEPC where
    toField = toField . pack . show
-- | ParentID
type ParentID = LabelEPC

-- applies the string to the correct readURI function
-- i.e, figures out whether to return InstanceLabel or ClassLabel
readLabelEPC :: String -> Maybe Quantity -> Maybe LabelEPC
-- readLabelEPC = error "not implemented yet"
readLabelEPC epcStr mQt =
  case fmap (`CL` mQt) (readURIClassLabelEPC epcTokens) of
    Left _ -> case fmap IL (readURIInstanceLabelEPC epcTokens) of
      Left _ -> Nothing
      Right il -> Just il
    Right a -> Just a
  where
    epcTokens = splitOn ":" epcStr

  -- case epcTokens of
  --   ("urn" : "epc" : "class" : "lgtin" : _) ->
  --     Just $ CL (getRightOrError $ readURIClassLabelEPC epcTokens) mQt
  --   ("urn" : "epc" : "id" : "grai" : _) ->
  --     Just $ CL (getRightOrError $ readURIClassLabelEPC epcTokens) mQt
  --   _ -> Just $ IL $ getRightOrError $ readURIInstanceLabelEPC epcTokens


-- |The What dimension specifies what physical or digital objects
-- participated in the event
data DWhat = -- ObjectDWhat action epcList quantityList
             ObjectDWhat Action [LabelEPC]
           -- AggregationDWhat action parentID childEPC
           | AggregationDWhat Action (Maybe ParentID) [LabelEPC]
           -- TransactionDWhat action parentID(URI) bizTransactionList epcList
           | TransactionDWhat Action (Maybe ParentID) [BizTransaction] [LabelEPC]
           -- TransformationDWhat transformationID inputEPCList outputEPCList
           | TransformationDWhat (Maybe TransformationID) [LabelEPC]  [LabelEPC]
           deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions ''DWhat)
instance ToSchema DWhat

instance ToField DWhat where
  toField = toField . TxtL.toStrict . encodeToLazyText

ppDWhat :: DWhat -> String
ppDWhat (ObjectDWhat a epcs) =
  "OBJECT WHAT\n" ++ show a ++ "\n" ++ show epcs ++ "\n"
ppDWhat (AggregationDWhat a pid epcs ) =
  "AGGREGATION WHAT\n" ++ show a ++ "\n" ++ show pid ++ "\n" ++ show epcs ++ "\n"
ppDWhat (TransactionDWhat a s bizT epcs ) =
  "TRANSACTION WHAT\n" ++ show a ++ "\n" ++ show s ++ "\n" ++ show bizT ++ "\n" ++ show epcs ++ "\n"
ppDWhat (TransformationDWhat tid inputEpcs outputEpcs ) =
  "TRANSFORMATION WHAT\n" ++ show tid ++ "\n" ++ show inputEpcs ++ "\n" ++ show outputEpcs ++ "\n"

makeClassy ''DWhat
