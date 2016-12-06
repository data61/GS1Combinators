module Data.GS1.Object where

--import Control.Lens.TH
import Data.GS1.EPC
import Data.GS1.URI

-- |EPCIS 1.0
data Object = PhysicalObject { _id :: ObjectID } | DigitalObject { _id :: ObjectID }

-- |The ObjectID
-- |Ref: CBV 8.2 & 8.3, EPCIS 1.0
data ObjectID = InstanceLevelID {
                 _epcList    :: [EPC]
               , _parentID   :: ObjectID
               , _childEPCs  :: [EPC]
               , _inputEPCS  :: [EPC]
               , _outputEPCS :: [EPC]
               }
               | ClassLevelID {
                 _epcClass :: EpcClass
               }

-- |Any identifiable object will be identified by ObjectID
class Identify a where
  ident :: a -> ObjectID

-- |Object is an instance of Identify
instance Identify Object where
  ident (PhysicalObject id) = id
  ident (DigitalObject id) = id
