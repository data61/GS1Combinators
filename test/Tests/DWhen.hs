module Tests.DWhen (
  testParseTime
) where

import Data.Either.Combinators
import Data.Time
import Test.Hspec
import Data.GS1.EPC
import Data.GS1.DWhen
import Data.GS1.Parser.Parser

type EitherET  = Either ParseFailure EPCISTime
type EitherETZ = Either ParseFailure TimeZone

testParseTime :: Spec
testParseTime =
  describe "parse string to time" $ do
    it "parses the string to time with default format" $ do
      let zt = parseStr2Time "2005-04-03T20:33:31.116-06:00"
      show zt `shouldBe` "Right 2005-04-04 02:33:31.116 UTC"

    it "parses the string to time with default format" $ do
      let z = parseStr2TimeZone "-06:00"
      show z `shouldBe` "Right -0600"

    it "parses slightly invalid string and returns ParseFailure" $ do
      let zt = fromLeft' (parseStr2Time "2005-04-3T20:33:31.116-06:00" :: EitherET)
      zt `shouldBe` TimeZoneError

    it "parses empty string and returns ParseFailure" $ do
      let zt = fromLeft' (parseStr2Time "" :: EitherET)
      zt `shouldBe` TimeZoneError
