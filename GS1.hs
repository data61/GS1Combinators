module GS1 where

-- Verbs

data What = W [UID]
type When = EPCISTime
data Where = W ReadPointLocation BuisinessLocation
data ReadPointLocation = RP Location
data BuisinessLocation = B Location
data Why = W BusinessStep Disposition [BusinessTransactionReference] [SrcDestReference]

data BusinessStep = Accepting | Arriving | Assembling | Collecting 
    | Commissioning | Consigning | CreatingClassInstance | CycleCounting 
    | Decommissioning | Departing | Destroying | Disassembling | Dispensing | Encoding
    | EnteringExiting | Holding | Inspecting | Installing | Killing | Loading | Other 
    | Packing | Picking | Receiving | Removing | Repackaging | Repairing | Replacing
    | Reserving | RetailSelling | Shipping | StagingOutbound | StockTaking | Stocking 
    | Storing | Transporting | Unloading | VoidShipping

data Disposition = Active | ContainerClosed | Damaged | Destroyed | Dispensed | Encoded 
    | Expired | InProgress | InTransit | Inactive | NoPedgreeMatch | NonSellableOther 
    | PartiallyDispensed | Recalled | Reserved | RetailSold | Returned | SellableAccessible 
    | Stolen | Unknown

data BusinessTransactionReference = BTR BusinessTransactionType BusinessTransactionIdentifier

data BusinessTransactionType = Bol | Desadv | Inv | Pedigree | Po | Poc 
    | Prodorder | Recadv | Rma

data BusinessTransactionIdentifier =
    EPCPureIdentity uri | URN uri | Http uri  --probably need to make explicit constructors for these

data SrcDestReference = SDR SrcDestType Where 

data SrcDestTypeURI = SDTU Payload

data SrcDestType = OwningParty | PossessingParty  


