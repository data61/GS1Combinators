{-# LANGUAGE DeriveGeneric #-}

module Data.GS1 where

import GHC.Generics

-- Verbs

newtype What = What [UID] deriving (Show,Eq,Generic)
type    When = EPCISTime
data    Where = Where ReadPointLocation BuisinessLocation deriving (Show,Eq,Generic)
newtype ReadPointLocation = RP Location deriving (Show,Eq,Generic)
newtype BuisinessLocation = B Location deriving (Show,Eq,Generic)
data    Why = Why BusinessStep Disposition [BusinessTransactionReference] [SrcDestReference] deriving (Show,Eq,Generic)

-- TODO implement these
data UID       = UID       deriving (Show,Eq,Generic)
data Location  = Location  deriving (Show,Eq,Generic)
data EPCISTime = EPCISTime deriving (Show,Eq,Generic)
data Payload   = Payload   deriving (Show,Eq,Generic)

data BusinessStep = Accepting | Arriving | Assembling | Collecting
    | Commissioning | Consigning | CreatingClassInstance | CycleCounting
    | Decommissioning | Departing | Destroying | Disassembling | Dispensing | Encoding
    | EnteringExiting | Holding | Inspecting | Installing | Killing | Loading | Other
    | Packing | Picking | Receiving | Removing | Repackaging | Repairing | Replacing
    | Reserving | RetailSelling | Shipping | StagingOutbound | StockTaking | Stocking
    | Storing | Transporting | Unloading | VoidShipping
    deriving (Show,Eq,Generic)

data Disposition = Active | ContainerClosed | Damaged | Destroyed | Dispensed | Encoded
    | Expired | InProgress | InTransit | Inactive | NoPedgreeMatch | NonSellableOther
    | PartiallyDispensed | Recalled | Reserved | RetailSold | Returned | SellableAccessible
    | Stolen | Unknown
    deriving (Show,Eq,Generic)

data BusinessTransactionReference = BTR BusinessTransactionType (BusinessTransactionIdentifier ())
  deriving (Show,Eq,Generic)

data BusinessTransactionType = Bol | Desadv | Inv | Pedigree | Po | Poc
    | Prodorder | Recadv | Rma
    deriving (Show,Eq,Generic)

-- NOTE what is uri supposed to represent?
data BusinessTransactionIdentifier uri =
    EPCPureIdentity uri | URN uri | Http uri  --probably need to make explicit constructors for these
    deriving (Show,Eq,Generic)

data SrcDestReference = SDR SrcDestType Where deriving (Show,Eq,Generic)

data SrcDestTypeURI = SDTU Payload deriving (Show,Eq,Generic)

data SrcDestType = OwningParty | PossessingParty deriving (Show,Eq,Generic)
