{-# LANGUAGE DeriveGeneric #-}

module Data.GS1.Event where
import           GHC.Generics
import           Data.GS1.Location
import           Data.GS1.Object
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.LocalTime


{-
   A timestamp, giving the date and time in a time zone-independent manner.
   For bindings in which fields of this type are represented textually,
   an ISO-8601 compliant representation SHOULD be used.
-}
type EPCISTime = UniversalTime

data BusinessStep = Accepting | Arriving | Assembling | Collecting
    | Commissioning | Consigning | Creating_Class_Instance | Cycle_Counting
    | Decommissioning | Departing | Destroying | Disassembling | Dispensing | Encoding
    | Entering_Exiting | Holding | Inspecting | Installing | Killing | Loading | Other
    | Packing | Picking | Receiving | Removing | Repackaging | Repairing | Replacing
    | Reserving | Retail_Selling | Shipping | Staging_Outbound | Stock_Taking | Stocking
    | Storing | Transporting | Unloading | Void_Shipping
    deriving (Show,Eq,Generic)

data Disposition = Active | Container_Closed | Damaged | Destroyed | Dispensed | Encoded
    | Expired | In_Progress | In_Transit | Inactive | No_Pedgree_Match | Non_Sellable_Other
    | Partially_Dispensed | Recalled | Reserved | Retail_Sold | Returned | Sellable_Accessible
    | Sellable_Not_Accessible | Stolen | Unknown
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
dispositionValidList Container_Closed =  [Staging_Outbound]
dispositionValidList Damaged          =  [Accepting, Inspecting, Receiving, Removing, Repairing, Replacing]
dispositionValidList Destroyed  =  [Destroying]
dispositionValidList Dispensed  =  [] -- nothing defined - page 25 of spec
dispositionValidList Encoded    =  [Encoding]
dispositionValidList Expired    =  [Holding, Staging_Outbound, Storing]
dispositionValidList In_Progress=  [Receiving, Picking, Loading, Accepting, Staging_Outbound, Arriving, Void_Shipping]
dispositionValidList In_Transit =  [Shipping, Departing]
dispositionValidList Inactive   =  [Decommissioning]
dispositionValidList No_Pedgree_Match   =  [Holding, Staging_Outbound, Storing]
dispositionValidList Non_Sellable_Other =  [Holding, Inspecting, Staging_Outbound, Storing]
dispositionValidList Partially_Dispensed=  []  -- nothing defined - page 25 of spec
dispositionValidList Recalled   =  [Holding, Staging_Outbound, Storing]
dispositionValidList Reserved   =  [Reserving]
dispositionValidList Retail_Sold=  [Retail_Selling]
dispositionValidList Returned   =  [Receiving, Holding, Shipping]
dispositionValidList Sellable_Accessible     =  [Stocking, Receiving]
dispositionValidList Sellable_Not_Accessible =  [Receiving, Storing, Loading, Holding, Inspecting]
dispositionValidList Stolen =  [] -- nothing defined - page 25 of spec
dispositionValidList Unknown =  [] -- nothing defined - page 25 of spec

dispositionValidFor :: BusinessStep -> Disposition -> Bool
dispositionValidFor bs disp = bs `elem` dispositionValidList disp

type TransformationID = String -- FIXME user defined element
type SrcDestID = String -- FIXME user defined element
data SrcDestType = OwningParty SrcDestID | PossessingParty SrcDestID
  deriving (Show,Eq,Generic)


data When = When {
    _eventTime :: EPCISTime,
    _recordTime :: EPCISTime,
    _timeZone :: TimeZone
                 } deriving (Show,Eq,Generic)


data Where = Where {
  _readPoint   :: (Maybe ReadPointLocation),
  _bizLocation :: (Maybe BusinessLocation),
  _srcDestType :: (Maybe [SrcDestType])
} deriving (Show,Eq,Generic)

data What = ObjectWhat {
              _objects :: [ObjectID],
              _action  :: Action,
              _btt     :: [BusinessTransactionType],
              _ilmd    :: Maybe Ilmd
            }
          | AggregationWhat {
              _parentID :: Maybe ObjectID,
              _objects  :: [ObjectID],
              _action   :: Action,
              _btt      :: [BusinessTransactionType]
            }
          | QuantityWhat {
              _objects  :: [ObjectID],
              _btt      :: [BusinessTransactionType]
            }
          | TransformationWhat {
              _input    :: [ObjectID],
              _output   :: [ObjectID],
              _transformationID :: TransformationID,
              _btt      :: [BusinessTransactionType],
              _ilmd    :: Maybe Ilmd
             }
          | TransactionWhat {
              _parentID :: Maybe ObjectID,
              _objects  :: [ObjectID],
              _action   :: Action,
              _btt      :: [BusinessTransactionType]
            } deriving (Show,Eq,Generic)


data EventType = ObjectEvent | AggregationEvent | QuantityEvent |
  TransactionEvent | TransformationEvent  deriving (Show,Eq,Generic)

type EventID = Int --FIXME - user defined element

data Action = Add | Observe | Delete  deriving (Show,Eq,Generic)

data Event = Event {
  _type :: EventType,
  _id   :: EventID,
  _what :: What,
  _when :: When,
  _why :: Why,
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
  _businessStep :: (Maybe BusinessStep),
  _disposition  :: (Maybe Disposition)
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

