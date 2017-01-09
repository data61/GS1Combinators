{-# LANGUAGE DeriveGeneric #-}

module Data.GS1 where

import           Data.GS1.Location
import           Data.GS1.Event
import           GHC.Generics

-- TODO implement these
data URI = URI deriving (Show,Eq,Generic)-- URN Namespace Payload |EPC |URI Namespace Payload deriving (Show,Eq,Generic)
type Payload = String




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
