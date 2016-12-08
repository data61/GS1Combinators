{-# LANGUAGE DeriveGeneric #-}

module Data.GS1.Event where
import           Data.GS1.Location
import           Data.GS1.Object
import           Data.GS1.URI
import           Data.List
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.LocalTime
import           GHC.Generics


{-
   A timestamp, giving the date and time in a time zone-independent manner.
   For bindings in which fields of this type are represented textually,
   an ISO-8601 compliant representation SHOULD be used.
-}
type EPCISTime = UniversalTime

data BusinessStep = Accepting
                  | Arriving
                  | Assembling
                  | Collecting
                  | Commissioning
                  | Consigning
                  | CreatingClassInstance
                  | CycleCounting
                  | Decommissioning
                  | Departing
                  | Destroying
                  | Disassembling
                  | Dispensing
                  | Encoding
                  | EnteringExiting
                  | Holding
                  | Inspecting
                  | Installing
                  | Killing
                  | Loading
                  | Other
                  | Packing
                  | Picking
                  | Receiving
                  | Removing
                  | Repackaging
                  | Repairing
                  | Replacing
                  | Reserving
                  | RetailSelling
                  | Shipping
                  | StagingOutbound
                  | StockTaking
                  | Stocking
                  | Storing
                  | Transporting
                  | Unloading
                  | VoidShipping
    deriving (Show,Eq,Generic)

ppBusinessStep :: BusinessStep -> String
ppBusinessStep bizStep = case bizStep of
                           Accepting             -> "accepting"
                           Arriving              -> "arriving"
                           Assembling            -> "assembling"
                           Collecting            -> "collecting"
                           Commissioning         -> "commissioning"
                           Consigning            -> "consigning"
                           CreatingClassInstance -> "creating_class_instance"
                           CycleCounting         -> "cycle_counting"
                           Decommissioning       -> "decommissioning"
                           Departing             -> "departing"
                           Destroying            -> "destroying"
                           Disassembling         -> "disassembling"
                           Dispensing            -> "dispensing"
                           Encoding              -> "encoding"
                           EnteringExiting       -> "entering_exiting"
                           Holding               -> "holding"
                           Inspecting            -> "inspecting"
                           Installing            -> "installing"
                           Killing               -> "killing"
                           Loading               -> "loading"
                           Other                 -> "other"
                           Packing               -> "packing"
                           Receiving             -> "receiving"
                           Removing              -> "removing"
                           Repackaging           -> "repackaging"
                           Repairing             -> "repairing"
                           Replacing             -> "replacing"
                           Reserving             -> "reserving"
                           RetailSelling         -> "retail_selling"
                           Shipping              -> "shipping"
                           StagingOutbound       -> "staging_outbound"
                           StockTaking           -> "stock_taking"
                           Stocking              -> "stocking"
                           Storing               -> "storing"
                           Transporting          -> "transporting"
                           Unloading             -> "unloading"
                           VoidShipping          -> "void_shipping"

instance URI BusinessStep where
  ppURI bizStep      = intercalate ":" ["urn:epcglobal:cbv", "bizstep", ppBusinessStep bizStep]
  uriPrefix _        = "urn:epcglobal:cbv"
  uriQuantifier _    = "bizstep"
  uriPayload bizStep = ppBusinessStep bizStep

data Disposition = Active | ContainerClosed | Damaged | Destroyed | Dispensed | Encoded
    | Expired | InProgress | InTransit | Inactive | NoPedgreeMatch | NonSellableOther
    | PartiallyDispensed | Recalled | Reserved | RetailSold | Returned | SellableAccessible
    | SellableNotAccessible | Stolen | Unknown
    deriving (Show,Eq,Generic)

type BusinessTransactionIdentifier = Maybe String --FIXME - user defined element
data BusinessTransactionType = Bol BusinessTransactionIdentifier
                             | Desadv BusinessTransactionIdentifier
                             | Inv BusinessTransactionIdentifier
                             | Pedigree BusinessTransactionIdentifier
                             | Po BusinessTransactionIdentifier
                             | Poc BusinessTransactionIdentifier
                             | Prodorder BusinessTransactionIdentifier
                             | Recadv BusinessTransactionIdentifier
                             | Rma BusinessTransactionIdentifier
    deriving (Show,Eq,Generic)



-- Valid Dispositions, defined in section CBV 7.2
dispositionValidList :: Disposition -> [BusinessStep]
dispositionValidList Active           =  [Commissioning]
dispositionValidList ContainerClosed =  [StagingOutbound]
dispositionValidList Damaged          =  [Accepting, Inspecting, Receiving, Removing, Repairing, Replacing]
dispositionValidList Destroyed  =  [Destroying]
dispositionValidList Dispensed  =  [] -- nothing defined - page 25 of spec
dispositionValidList Encoded    =  [Encoding]
dispositionValidList Expired    =  [Holding, StagingOutbound, Storing]
dispositionValidList InProgress=  [Receiving, Picking, Loading, Accepting, StagingOutbound, Arriving, VoidShipping]
dispositionValidList InTransit =  [Shipping, Departing]
dispositionValidList Inactive   =  [Decommissioning]
dispositionValidList NoPedgreeMatch   =  [Holding, StagingOutbound, Storing]
dispositionValidList NonSellableOther =  [Holding, Inspecting, StagingOutbound, Storing]
dispositionValidList PartiallyDispensed=  []  -- nothing defined - page 25 of spec
dispositionValidList Recalled   =  [Holding, StagingOutbound, Storing]
dispositionValidList Reserved   =  [Reserving]
dispositionValidList RetailSold=  [RetailSelling]
dispositionValidList Returned   =  [Receiving, Holding, Shipping]
dispositionValidList SellableAccessible     =  [Stocking, Receiving]
dispositionValidList SellableNotAccessible =  [Receiving, Storing, Loading, Holding, Inspecting]
dispositionValidList Stolen =  [] -- nothing defined - page 25 of spec
dispositionValidList Unknown =  [] -- nothing defined - page 25 of spec

dispositionValidFor :: BusinessStep -> Disposition -> Bool
dispositionValidFor bs disp = bs `elem` dispositionValidList disp

type TransformationID = String -- FIXME user defined element
type SrcDestID = String -- FIXME user defined element
data SrcDestType = OwningParty SrcDestID | PossessingParty SrcDestID
  deriving (Show,Eq,Generic)


data When = When {
    eventTime  :: EPCISTime,
    recordTime :: EPCISTime,
    timeZone   :: TimeZone
                 } deriving (Show,Eq,Generic)


data Where = Where {
  readPoint   :: (Maybe ReadPointLocation),
  bizLocation :: (Maybe BusinessLocation),
  srcDestType :: (Maybe [SrcDestType])
} deriving (Show,Eq,Generic)

data What = ObjectWhat {
              objects :: [ObjectID],
              action  :: Action,
              btt     :: [BusinessTransactionType],
              ilmd    :: Maybe Ilmd
            }
          | AggregationWhat {
              parentID :: Maybe ObjectID,
              objects  :: [ObjectID],
              action   :: Action,
              btt      :: [BusinessTransactionType]
            }
          | QuantityWhat {
              objects :: [ObjectID],
              btt     :: [BusinessTransactionType]
            }
          | TransformationWhat {
              input            :: [ObjectID],
              output           :: [ObjectID],
              transformationID :: TransformationID,
              btt              :: [BusinessTransactionType],
              ilmd             :: Maybe Ilmd
             }
          | TransactionWhat {
              parentID :: Maybe ObjectID,
              objects  :: [ObjectID],
              action   :: Action,
              btt      :: [BusinessTransactionType]
            } deriving (Show,Eq,Generic)


data EventType = ObjectEvent | AggregationEvent | QuantityEvent |
  TransactionEvent | TransformationEvent  deriving (Show,Eq,Generic)

type EventID = Int --FIXME - user defined element

data Action = Add | Observe | Delete  deriving (Show,Eq,Generic)

data Event = Event {
  _type  :: EventType,
  _id    :: EventID,
  _what  :: What,
  _when  :: When,
  _why   :: Why,
  _where :: Where
} deriving (Show,Eq,Generic)


objectEvent :: EventID -> [ObjectID] -> Action -> [BusinessTransactionType] ->
    Maybe Ilmd ->When -> Why -> Where -> Event
objectEvent id objects action btt ilmd when why whre =
  Event ObjectEvent id (ObjectWhat objects action btt ilmd) when why whre

--TODO: check parent is present when needed (based on action)
aggregationEvent :: EventID -> Maybe ObjectID -> [ObjectID] -> Action ->
  [BusinessTransactionType] -> When -> Why -> Where -> Event
aggregationEvent id parent objects action btt when why whre =
  Event AggregationEvent id (AggregationWhat parent objects action btt) when why whre

--TODO: check that all ObjectIDs are class objects with quantities.
quantityEvent :: EventID -> [ObjectID] -> [BusinessTransactionType] ->
  When -> Why -> Where -> Event
quantityEvent id objects btt when why whre=
  Event QuantityEvent id (QuantityWhat objects btt) when why whre

transformationEvent :: EventID -> [ObjectID] -> [ObjectID] -> TransformationID
                    -> [BusinessTransactionType] -> Maybe Ilmd -> When -> Why ->
                      Where -> Event
transformationEvent id inputs outputs transformID btt  ilmd when why whre =
  Event TransformationEvent id (TransformationWhat inputs outputs transformID btt ilmd)
    when why whre

transactionEvent :: EventID -> Maybe ObjectID -> [ObjectID] -> Action ->
  [BusinessTransactionType] -> When -> Why -> Where -> Event
transactionEvent id parentID objects action btt when why whre =
  Event TransactionEvent id (TransactionWhat parentID objects action btt) when why whre

data Why = Why  {
  businessStep :: (Maybe BusinessStep),
  disposition  :: (Maybe Disposition)
} deriving (Show,Eq,Generic)

-- The why smart constructor
-- Have to make sure the disposition is valid for that particular business
-- step.
-- FIXME: do we care if the businessStep and disposition match if one of them is Nothing?
why :: Maybe BusinessStep -> Maybe Disposition -> Why
why step disp
    |isJust step && isJust disp =
      if dispositionValidFor (fromJust step) (fromJust disp)
      then (Why step disp)
      else error $ "Disposition not valid for business step. " ++
        " Valid BusinessSteps for " ++ show (fromJust disp) ++ "include: " ++
                      show (dispositionValidList (fromJust disp))
    |otherwise = (Why step disp)



{--
== Object Event ==
 An ObjectEvent Captures information about an event pertaining to one or more physical or digital objects identified by instance-level (EPC) or class-level (EPC Class) identifiers.
 While more than one EPC and/or EPC Class may appear in an ObjectEvent, no relationship or association between those objects i implied other than the coincidence of having experienced identical events in the real world
*denotes optional field
 - Fields -
 When - EventTime/RecordTime/EventTimeOffset
 What - EpcList* quantityList* (an ObjectEvent shall contain a non-empty epcList or a non-empty quantityList or both)
        Action
        Ilmd
        [BusinessTransactionType]*
 Why -
        BusinessStep*
        Disposition*

 Where -
        ReadPoint*
        BusinessLocation*
        [SrcDestType]*
--}

{--
== Aggregation Event ==

 - Fields -
 When - EventTime/RecordTime/EventTimeOffset
 What -
        ParentID** (Optional when Action == Observe, required otherwise)
        [ChildEPC]* (An AggregationEvent SHALL contain either a non-empty [childEPCs] a non-empty [childQuantity], or both, except that both may be empty if Action==Delete)
        [ChildQuantity]*
        Action
        [BusinessTransactionType]
 Why -
        BusinessStep*
        Disposition*
 Where -
        ReadPoint*
        BusinessLocation*
        [SrcDestType]
--}

{--
== Quantity Event ==
 - Fields -
 When - EventTime/RecordTime/EventTimeOffset
 What -
        EPCClass, Quantity
        [BusinessTransactionType]
 Why -
        BusinessStep*
        Disposition*
 Where -
        ReadPoint*
        BusinessLocation*
--}

{--
== Transaction Events ==
 - Fields -
 When - EventTime/RecordTime/EventTimeOffset
 What -
        [BusinessTransactionType]
        ParentID*
        EPCList, Quantity List
        Action
 Why -
        BusinessStep
        Disposition

 Where -
        ReadPoint
        BusinessLocation
        [SrcDestType]
--}

{--
== Transformation Events ==
 - Fields -
 When - EventTime/RecordTime/EventTimeOffset
 What - [InputEPC]*, [OutputQuantity]*
        [OutputEPC]*, [OutputQuantity]*
        TransformationID*
        [BusinessTransactionType]
        Ilmd*
 Why  -
        BusinessStep*
        Disposition*
 Where -
        ReadPoint*
        BusinessLocation*
        [SrcDestType]*
--}

