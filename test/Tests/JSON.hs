{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Tests.JSON where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Text ( Text )
import Data.Time.Clock ( UTCTime(..) )
import Data.Time.LocalTime ( TimeZone(..) )
import Data.Time.Calendar ( Day(..) )
import Test.Hspec

import Data.GS1.EPC
import Data.GS1.Event
import Data.GS1.DWhat
import Data.GS1.DWhere
import Data.GS1.DWhen ( DWhen(..) ) -- also imports orphan TimeZone To/FromJSON instances
import Data.GS1.DWhy


testJSON :: Spec
testJSON = do
  toFromJSONSpec "TimeZone" timeZoneJSON timeZone
  toFromJSONSpec "EPCISTime" epcisTimeJSON epcisTime
  toFromJSONSpec "SourceLocation" sourceLocationJSON sourceLocation
  toFromJSONSpec "DestinationLocation" destinationLocationJSON destinationLocation
  
  toFromJSONSpec "ObjectEvent-IL"
                 (eventJSON "ObjectEvent" objectWhatILProps)
                 (event ObjectEventT objectWhatIL)

  toFromJSONSpec "ObjectEvent-CL"
                 (eventJSON "ObjectEvent" objectWhatCLProps)
                 (event ObjectEventT objectWhatCL)
                 
                 
  toFromJSONSpec "AggregationEvent-IL"
                 (eventJSON "AggregationEvent" aggregationWhatILProps)
                 (event AggregationEventT aggregationWhatIL)

  toFromJSONSpec "AggregationEvent-CL"
                 (eventJSON "AggregationEvent" aggregationWhatCLProps)
                 (event AggregationEventT aggregationWhatCL)


   
toFromJSONSpec :: (ToJSON a, FromJSON a, Show a, Eq a) => String -> Value -> a -> SpecWith ()
toFromJSONSpec s v x =
  describe s $ do
    parsing v $ \a -> a `shouldBe` x
    writing x $ \a -> a `shouldBe` v

parsing :: FromJSON a => Value -> (a -> Expectation) -> SpecWith ()
parsing x f = it "can be parsed from JSON" $
  case fromJSON x of
    Error e -> expectationFailure $ "no parse: " <> e
    Success a' -> f a'

writing :: ToJSON a => a -> (Value -> Expectation) -> SpecWith ()
writing x f =  it "can be written as JSON" $
  f $ toJSON x


-- Test Values and expected JSON

event :: EventType -> DWhat -> Event
event t w = Event t Nothing w when why where'

eventJSON :: Text -> [Pair] -> Value
eventJSON t w = object $ [ "isA" .= String t
                         ] <> w
                           <> whenProps
                           <> whyProps
                           <> whereProps


objectWhatIL :: DWhat
objectWhatIL = ObjWhat $ ObjectDWhat Add [ IL instanceLabelEPC_1234 ]

objectWhatILProps :: [Pair]
objectWhatILProps = [ "action" .= String "ADD"
                    , "epcList" .= [ instanceLabelEPCJSON_1234 ]
                    ]

objectWhatCL :: DWhat
objectWhatCL = ObjWhat $ ObjectDWhat Delete [ CL classLabelEPC_321 (Just $ ItemCount 100) ]

objectWhatCLProps :: [Pair]
objectWhatCLProps = [ "action" .= String "DELETE"
                    , "quantityList" .= [ object [ "epcClass" .= classLabelEPCJSON_321
                                                 , "quantity" .= Number 100
                                                 ]
                                        ]
                    ]


aggregationWhatIL :: DWhat
aggregationWhatIL = AggWhat $ AggregationDWhat Delete (Just $ ParentLabel instanceLabelEPC_1234) [ IL instanceLabelEPC_5678 ]

aggregationWhatILProps :: [Pair]
aggregationWhatILProps = [ "action" .= String "DELETE"
                         , "parentID" .= instanceLabelEPCJSON_1234
                         , "childEPCs" .= [ instanceLabelEPCJSON_5678 ]
                         ]

                         

aggregationWhatCL :: DWhat
aggregationWhatCL = AggWhat $ AggregationDWhat Delete (Just $ ParentLabel instanceLabelEPC_1234) [ CL classLabelEPC_654 (Just $ ItemCount 19) ]

aggregationWhatCLProps :: [Pair]
aggregationWhatCLProps = [ "action" .= String "DELETE"
                         , "parentID" .= instanceLabelEPCJSON_1234
                         , "childQuantityList" .= [ object [ "epcClass" .= classLabelEPCJSON_654
                                                           , "quantity" .= Number 19
                                                           ]
                                                  ]
                         ]
                       

when :: DWhen
when = DWhen epcisTime (Just epcisTime) timeZone

whenProps :: [Pair]
whenProps = [ "eventTime" .= epcisTimeJSON
            , "recordTime" .= Just epcisTimeJSON
            , "eventTimeZoneOffset" .= timeZoneJSON
            ]

why :: DWhy
why = DWhy (Just bizStep) (Just disposition)

whyProps :: [Pair]
whyProps = [ "bizStep" .= bizStepJSON
           , "disposition" .= dispositionJSON
           ]

where' :: DWhere
where' = DWhere (Just $ ReadPointLocation location)
                (Just $ BizLocation location)
                [ sourceLocation ]
                [ destinationLocation ]

whereProps :: [Pair]
whereProps = [ "sourceList" .= [ sourceLocationJSON ]
             , "destinationList" .= [ destinationLocationJSON ]
             , "readPoint" .= locationJSON
             , "bizLocation" .= locationJSON
             ]

mkInstanceLabelEPC :: Text -> InstanceLabelEPC
mkInstanceLabelEPC = SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") . SerialNumber

mkInstanceLabelEPCJSON :: Text -> Value
mkInstanceLabelEPCJSON = String . mappend "urn:epc:id:sgtin:0614141.107346."

mkInstanceLabel :: Text -> (InstanceLabelEPC, Value)
mkInstanceLabel x = (mkInstanceLabelEPC x, mkInstanceLabelEPCJSON x) 

(instanceLabelEPC_1234, instanceLabelEPCJSON_1234) = mkInstanceLabel "1234"
(instanceLabelEPC_5678, instanceLabelEPCJSON_5678) = mkInstanceLabel "5678"


mkClassLabelEPC :: Text -> ClassLabelEPC
mkClassLabelEPC = LGTIN (GS1CompanyPrefix "0614141") (ItemReference "107366") . Lot

mkClassLabelEPCJSON :: Text -> Value
mkClassLabelEPCJSON = String . mappend "urn:epc:class:lgtin:0614141.107366."

mkClassLabel :: Text -> (ClassLabelEPC, Value)
mkClassLabel x = (mkClassLabelEPC x, mkClassLabelEPCJSON x)

(classLabelEPC_321, classLabelEPCJSON_321) = mkClassLabel "321"
(classLabelEPC_654, classLabelEPCJSON_654) = mkClassLabel "654"
                      

location :: LocationEPC
location = (SGLN (GS1CompanyPrefix "0614141") (LocationReference "07346") (Just (SGLNExtension "1234")))

locationJSON :: Value
locationJSON = String "urn:epc:id:sgln:0614141.07346.1234"

sourceLocation :: SourceLocation
sourceLocation = SourceLocation sourceOrDestType location

sourceLocationJSON :: Value
sourceLocationJSON = object [ "type" .= sourceOrDestTypeJSON
                            , "source" .= locationJSON
                            ]

destinationLocation :: DestinationLocation
destinationLocation = DestinationLocation sourceOrDestType location

destinationLocationJSON :: Value
destinationLocationJSON = object [ "type" .= sourceOrDestTypeJSON
                                 , "destination" .= locationJSON
                                 ]

timeZone :: TimeZone
timeZone = TimeZone 600 False ""

timeZoneJSON :: Value
timeZoneJSON = String "+10:00"

epcisTime :: EPCISTime
epcisTime = EPCISTime $ UTCTime (ModifiedJulianDay 58564) 0

epcisTimeJSON :: Value
epcisTimeJSON = String ( "2019-03-22T00:00:00Z" )

bizStep :: BizStep
bizStep = Accepting

bizStepJSON :: Value
bizStepJSON = String "urn:epcglobal:cbv:bizstep:accepting"

disposition :: Disposition
disposition = Damaged

dispositionJSON :: Value
dispositionJSON = String "urn:epcglobal:cbv:disp:damaged"

sourceOrDestType :: SourceDestType
sourceOrDestType = SDOwningParty

sourceOrDestTypeJSON :: Value
sourceOrDestTypeJSON = String "urn:epcglobal:cbv:sdt:owning_party"
