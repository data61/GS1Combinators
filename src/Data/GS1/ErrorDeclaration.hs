{-# LANGUAGE DeriveGeneric #-}

module Data.GS1.ErrorDeclaration where

import           Data.GS1.URI
import           GHC.Generics

-- |EPCIS 1.2 section 7.5
-- FIXME example should be found to verify the implementation is correct
data ErrorReasonID = DidNotOccur
                   | IncorrectData
                   deriving (Show, Eq, Generic)

ppErrorReasonID :: ErrorReasonID -> String
ppErrorReasonID e = case e of
                      DidNotOccur   -> "did_not_occur"
                      IncorrectData -> "incorrect_data"

data ErrorDeclaration = ErrorDeclaration ErrorReasonID
  deriving (Show, Eq, Generic)

instance URI ErrorDeclaration where
  uriPrefix _                     = "urn:epcglobal:cbv"
  uriQuantifier _                 = "er"
  uriPayload (ErrorDeclaration r) = ppErrorReasonID r


