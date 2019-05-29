module Tests.DWhen (
  testParseTime
) where

import           Data.Aeson ( toJSON, Value(String) )
import           Data.Either.Combinators
import           Data.GS1.EPC
import           Data.GS1.Parser.Parser
import           Data.Time.LocalTime
import           Test.Hspec

testParseTime :: Spec
testParseTime = do
  describe "parse string to time" $ do
    it "parses the string to time with default format" $ do
      let zt = parseStr2Time "2005-04-03T20:33:31.116-06:00"
      show zt
        `shouldBe`
          "Right (EPCISTime {unEPCISTime = 2005-04-04 02:33:31.116 UTC})"

    it "parses the string to time with default format" $ do
      let z = parseStr2TimeZone "-06:00"
      show z `shouldBe` "Right -0600"

    it "parses slightly invalid string and returns ParseFailure" $ do
      let zt = fromLeft' (parseStr2Time "2005-04-3T20:33:31.116-06:00")
      zt `shouldBe` TimeZoneError (XMLSnippet "2005-04-3T20:33:31.116-06:00")

    it "parses empty string and returns ParseFailure" $ do
      let zt = fromLeft' (parseStr2Time "")
      zt `shouldBe` TimeZoneError (XMLSnippet "")

  describe "write time to string" $ do
    it "writes the timezone as [+-]hh:mm" $ do
      toJSON (TimeZone 600 False "AEST") `shouldBe` (String "+10:00")
      
      toJSON (TimeZone (-180) False "ADT") `shouldBe` (String "-03:00")
