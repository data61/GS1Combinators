{-# LANGUAGE DeriveGeneric #-}

module Data.GS1.ErrorDeclaration where

import           Data.GS1.EPCISTime
import           Data.GS1.EventID
import           Data.GS1.URI
import           Data.GS1.Utils
import           GHC.Generics

-- |EPCIS 1.2 section 7.5
-- FIXME example should be found to verify the implementation is correct
data ErrorReasonID = DidNotOccur
                   | IncorrectData
                   deriving (Show, Eq, Generic)

ppErrorReasonID :: ErrorReasonID -> String
ppErrorReasonID = revertCamelCase . show

data ErrorDeclaration = ErrorDeclaration
  {
    _declarationTime    :: EPCISTime
  , _reason             :: Maybe ErrorReasonID
  , _correctiveEventIDs :: Maybe [EventID]
  }
  deriving (Show, Eq, Generic)

instance URI ErrorDeclaration where
  uriPrefix _                         = "urn:epcglobal:cbv"
  uriQuantifier _                     = "er"
  uriPayload (ErrorDeclaration _ r _) = case r of
                                          Just a  -> ppErrorReasonID a
                                          Nothing -> ""
