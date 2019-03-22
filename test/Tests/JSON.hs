{-# LANGUAGE TypeApplications #-}

module Tests.JSON where

import Data.Aeson
import Data.Aeson.Types (Pair)
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
  simpleJSONSpec "TimeZone" timeZoneJSON timeZone
  simpleJSONSpec "EPCISTime" epcisTimeJSON epcisTime
  simpleJSONSpec "SourceLocation" sourceLocationJSON sourceLocation
  simpleJSONSpec "DestinationLocation" destinationLocationJSON destinationLocation
  simpleJSONSpec "ObjectEvent" objectEventJSON objectEvent

   
simpleJSONSpec :: (ToJSON a, FromJSON a, Show a, Eq a) => String -> Value -> a -> SpecWith ()
simpleJSONSpec s v x =
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




objectEvent :: Event
objectEvent = Event ObjectEventT Nothing objectWhat when why where'

objectEventJSON :: Value
objectEventJSON = object $ [ "isA" .= String "ObjectEvent"                       
                           ] <> objectWhatProps
                             <> whenProps
                             <> whyProps
                             <> whereProps


objectWhat :: DWhat
objectWhat = ObjWhat $ ObjectDWhat Add []

objectWhatProps :: [Pair]
objectWhatProps = [ "action" .= String "ADD"
                  , "epcList" .= Array mempty
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

-- labelEPC :: LabelEPC
-- labelEPC = IL _ Nothing

-- labelEPCJSON :: Value
-- labelEPCJSON = object [ "epcClass" .= String "urn:epc:class:lgtin:4012345.012345.998877"
                      
--                       ]


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
