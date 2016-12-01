{-# LANGUAGE DeriveGeneric #-}

module Data.GS1 where

import           Data.GS1.Location
import           GHC.Generics

-- TODO implement these
data URI = URI deriving (Show,Eq,Generic)-- URN Namespace Payload |EPC |URI Namespace Payload deriving (Show,Eq,Generic)
type Payload = String

-- TODO use built-in time package
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

data BusinessTransactionReference = BTR BusinessTransactionType (BusinessTransactionIdentifier)
  deriving (Show,Eq,Generic)

data BusinessTransactionIdentifier = BusinessTransactionIdentifier deriving (Show,Eq,Generic) --FIXME

data BusinessTransactionType = Bol | Desadv | Inv | Pedigree | Po | Poc
    | Prodorder | Recadv | Rma
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
-- has a list of business steps it's valid for. This is why we have a smart
-- constructor for Why. It checks whether the disposition is valid for
-- the particular business step.
--



--data SrcDestReference = SDR SrcDestType Where deriving (Show,Eq,Generic)

--data SrcDestTypeURI = SDTU Payload deriving (Show,Eq,Generic)

-- These are the Source/Dest Type URI payloads
-- urn:epcglobal:cbv:sdt:payload
-- FIXME LOCATION
data SrcDestType = OwningParty | PossessingParty | Loc Location deriving (Show,Eq,Generic)


-- example
--

--myTime = EPCISTime

--myEvent :: Event
--myEvent = Event (W []) myTime (W (RP Location)
