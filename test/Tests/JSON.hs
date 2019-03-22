{-# LANGUAGE TypeApplications #-}

module Tests.JSON where

import Data.Aeson
import Data.Time.LocalTime ( TimeZone(..) )
import Test.Hspec

import Data.GS1.EPC
import Data.GS1.DWhere
import Data.GS1.DWhen () -- import orphan TimeZone To/FromJSON instances


testJSON :: Spec
testJSON = do
  testDWhere
  testDWhen


testDWhen :: Spec
testDWhen = do
  simpleJSONSpec "TimeZone" (String "+10:00") (TimeZone 600 False "")


testDWhere :: Spec
testDWhere = do
  simpleJSONSpec "SourceLocation" sourceLocationJSON sourceLocation
  simpleJSONSpec "DestinationLocation" destinationLocationJSON destinationLocation


 where
   location = (SGLN (GS1CompanyPrefix "0614141") (LocationReference "07346") (Just (SGLNExtension "1234")))
   locationJSON = String "urn:epc:id:sgln:0614141.07346.1234"
   
   sourceLocation = SourceLocation SDOwningParty location     
   sourceLocationJSON = object [ "type" .= String "urn:epcglobal:cbv:sdt:owning_party"
                               , "source" .= locationJSON
                               ]

   destinationLocation = DestinationLocation SDOwningParty location
   destinationLocationJSON = object [ "type" .= String "urn:epcglobal:cbv:sdt:owning_party"
                                    , "destination" .= locationJSON
                                    ]                        

   
   
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
