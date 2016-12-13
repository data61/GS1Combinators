module Data.GS1.Object where

--import Control.Lens.TH
import           Data.GS1.EPC
import           Data.GS1.URI

-- |TODO expand it to the proper implementation when necessary
-- EPCIS Page 29
type Quantity = Integer

type Uom = String

-- |Simple quantity representation
data QuantityElement = QuantityElement EpcClass Quantity Uom
  deriving (Show, Eq)


-- |EPCIS 1.0
data Object = PhysicalObject { _id :: ObjectID } | DigitalObject { _id :: ObjectID }
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
               , _parentID   :: Maybe[ObjectID]
               , _childEPCs  :: [EPC]
               , _inputEPCS  :: [EPC]
               , _outputEPCS :: [EPC]
               , _ilmd       :: Ilmd
               }
               | ClassLevelID {
                 _epcClass :: EpcClass
               , _quantity :: QuantityElement
               , _ilmd     :: Ilmd
                              } deriving (Show, Eq)

-- |Any identifiable object will be identified by ObjectID
class Identify a where
  ident :: a -> ObjectID

-- |Object is an instance of Identify
instance Identify Object where
  ident (PhysicalObject id) = id
  ident (DigitalObject id)  = id
