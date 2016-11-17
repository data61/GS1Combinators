{-# LANGUAGE DeriveGeneric #-}

module Data.GS1 where

import GHC.Generics

data Event = Event What When Where Why

-- Verbs

newtype What = What [URI] deriving (Show,Eq,Generic)
type    When = EPCISTime
data    Where = Where ReadPointLocation BuisinessLocation deriving (Show,Eq,Generic)
newtype ReadPointLocation = RP Location deriving (Show,Eq,Generic)
newtype BuisinessLocation = Biz Location deriving (Show,Eq,Generic)
data    Why = Why BusinessStep Disposition [BusinessTransactionReference] [SrcDestReference] deriving (Show,Eq,Generic)

-- TODO implement these
data URI = URI deriving (Show,Eq,Generic)-- URN Namespace Payload |EPC |URI Namespace Payload deriving (Show,Eq,Generic)
type Payload = String
type Namespace = String -- registered IANA namespace
data Location  = Location  deriving (Show,Eq,Generic)
data EPCISTime = EPCISTime deriving (Show,Eq,Generic)

data BusinessStep = Accepting | Arriving | Assembling | Collecting
    | Commissioning | Consigning | Creating_Class_Instance | Cycle_Counting
    | Decommissioning | Departing | Destroying | Disassembling | Dispensing | Encoding
    | Entering_Exiting | Holding | Inspecting | Installing | Killing | Loading | Other
    | Packing | Picking | Receiving | Removing | Repackaging | Repairing | Replacing
    | Reserving | Retail_Selling | Shipping | Staging_Outbound | Stock_Taking | Stocking
    | Storing | Transporting | Unloading | Void_Shipping
    deriving (Show,Eq,Generic)

--show BusinessStep ~= urn:epcglobal:cbv:bizstep:BusinessStepConstructor

data Disposition = Active | Container_Closed | Damaged | Destroyed | Dispensed | Encoded
    | Expired | In_Progress | In_Transit | Inactive | No_Pedgree_Match | Non_Sellable_Other
    | Partially_Dispensed | Recalled | Reserved | Retail_Sold | Returned | Sellable_Accessible
    | Sellable_Not_Accessible | Stolen | Unknown
    deriving (Show,Eq,Generic)

data BusinessTransactionReference = BTR BusinessTransactionType (BusinessTransactionIdentifier ())
  deriving (Show,Eq,Generic)

data BusinessTransactionType = Bol | Desadv | Inv | Pedigree | Po | Poc
    | Prodorder | Recadv | Rma
    deriving (Show,Eq,Generic)

-- Valid Dispositions, defined in section 7.2
dispositionValidFor :: BusinessStep -> Disposition -> Bool
dispositionValidFor bs Active = bs `elem` [Commissioning]
dispositionValidFor bs Container_Closed = bs `elem` [Staging_Outbound]
dispositionValidFor bs Damaged = bs `elem` [Accepting, Inspecting, Receiving, Removing, Repairing, Replacing]
dispositionValidFor bs Destroyed = bs `elem` [Destroying]
dispositionValidFor bs Dispensed = bs `elem` [] -- nothing defined - page 25 of spec
dispositionValidFor bs Encoded = bs `elem` [Encoding] 
dispositionValidFor bs Expired = bs `elem` [Holding, Staging_Outbound, Storing] 
dispositionValidFor bs In_Progress = bs `elem` [Receiving, Picking, Loading, Accepting, Staging_Outbound, Arriving, Void_Shipping] 
dispositionValidFor bs In_Transit = bs `elem` [Shipping, Departing] 
dispositionValidFor bs Inactive = bs `elem` [Decommissioning] 
dispositionValidFor bs No_Pedgree_Match= bs `elem` [Holding, Staging_Outbound, Storing] 
dispositionValidFor bs Non_Sellable_Other = bs `elem` [Holding, Inspecting, Staging_Outbound, Storing] 
dispositionValidFor bs Partially_Dispensed = bs `elem` []  -- nothing defined - page 25 of spec
dispositionValidFor bs Recalled = bs `elem` [Holding, Staging_Outbound, Storing] 
dispositionValidFor bs Reserved = bs `elem` [Reserving] 
dispositionValidFor bs Retail_Sold = bs `elem` [Retail_Selling] 
dispositionValidFor bs Returned = bs `elem` [Receiving, Holding, Shipping] 
dispositionValidFor bs Sellable_Accessible = bs `elem` [Stocking, Receiving] 
dispositionValidFor bs Sellable_Not_Accessible = bs `elem` [Receiving, Storing, Loading, Holding, Inspecting] 
dispositionValidFor bs Stolen = bs `elem` [] -- nothing defined - page 25 of spec
dispositionValidFor bs Unknown = bs `elem` [] -- nothing defined - page 25 of spec

-- The why smart constructor
-- Have to make sure the disposition is valid for that particular business
-- step.
why :: BusinessStep -> Disposition -> [BusinessTransactionReference] -> [SrcDestReference] -> Maybe Why
why step disp trans srcdsts = if dispositionValidFor step disp 
                              then Just $ Why step disp trans srcdsts
                              else Nothing



-- NOTE what is uri supposed to represent?
data BusinessTransactionIdentifier uri =
    EPCPureIdentity uri | URN uri | Http uri  --probably need to make explicit constructors for these
    deriving (Show,Eq,Generic)


-- URIs!
-- There are two main classes of URIs. The ones defined in the EPCIS/GS1
-- standard, and user defined vocabluaries.
--
-- First, we define EPCIS URIs. They are defined as:
-- urn:epcglobal:cbv:qualifier:payload
-- where the qualifier can be one of: bizstep, disp, btt, sdt or er
-- These correspond to business step, disposition, business transaction type,
-- src/dest type or error reason
--
-- Not all dispositions can be used with all business steps. Each disposition
-- has a list of business steps it's valid for.
--



data SrcDestReference = SDR SrcDestType Where deriving (Show,Eq,Generic)

data SrcDestTypeURI = SDTU Payload deriving (Show,Eq,Generic)

data SrcDestType = OwningParty | PossessingParty deriving (Show,Eq,Generic)


-- example
--

myTime = EPCISTime

--myEvent :: Event
--myEvent = Event (W []) myTime (W (RP Location) 
