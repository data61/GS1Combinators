{-# LANGUAGE TemplateHaskell #-}

module Data.GS1.Object where

import           Control.Lens (lens, makeClassy)
import           Data.GS1.EPC
import           Data.GS1.URI

-- |TODO expand it to the proper implementation when necessary
-- EPCIS Page 29
type Quantity = Integer

type Uom = String

-- |Simple quantity representation
data QuantityElement = QuantityElement EPCClass Quantity Uom
  deriving (Show, Eq)

-- |EPCIS 7.3.6
-- Not sure if it is right way
-- ILMD is data that describes a specific instance of a physical or digital
-- object, or a specific batch/lot of objects that are produced in batches/lot.
type Ilmd = [String]

-- |The ObjectID
-- |Ref: CBV 8.2 & 8.3, EPCIS 1.0
data ObjectID = InstanceLevelID {
                 _epcList    :: [EPC]
               , _parentID   :: [ObjectID]
               , _childEPCs  :: [EPC]
               , _inputEPCS  :: [EPC]
               , _outputEPCS :: [EPC]
               , _ilmd       :: Ilmd
               }
                 | ClassLevelID {
                 _epcClass :: EPCClass
               , _quantity :: QuantityElement
               , _ilmd     :: Ilmd
               }
               deriving (Show, Eq)

makeClassy ''ObjectID

data IDLevel = InstanceLevelT
             | ClassLevelT
             deriving (Eq, Show)

-- |EPCIS 1.0
data Object = Object IDLevel ObjectID
  deriving (Eq, Show)

instance HasObjectID Object where
  objectID =
    lens (\(Object _ i) -> i)
         (\(Object t _) i -> let o = Object t i in
                                 case (t, i) of
                                   (InstanceLevelT, InstanceLevelID {}) -> o
                                   (ClassLevelT, InstanceLevelID {})    -> o)

