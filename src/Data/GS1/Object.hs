module Data.GS1.Object where

import Control.Lens.TH
import Data.GS1.URI

-- |EPCIS 1.0
data Object = PhysicalObject { _id :: ObjectID } | DigitalObject { _id :: ObjectID }

-- |The ObjectID
-- |Ref: CBV 8.2 & 8.3, EPCIS 1.0
data ObjectID = InstanceLevelID {
                 _epcList    :: [String]
               , _parentID   :: ObjectID
               , _childEPCs  :: [String]
               , _inputEPCS  :: [String]
               , _outputEPCS :: [String]
               }
               | ClassLevelID {
                 _epcClass :: String
               }

-- |Any identifiable object will be identified by ObjectID
class Identify a where
  ident :: a -> ObjectID

-- |Object is an instance of Identify
instance Identify Object where
  ident (PhysicalObject id) = id
  ident (DigitalObject id) = id
