module Tests.DWhen (
  testParseTime
) where

import Data.Either.Combinators
import Data.Time
import Test.Hspec
import Data.GS1.EPC
import Data.GS1.DWhen

type EitherET  = Either EPCISTimeError EPCISTime
type EitherETZ = Either EPCISTimeError TimeZone

testParseTime :: Spec
testParseTime =
  describe "parse string to time" $ do
    it "parses the string to time with default format" $ do
      let zt = fromRight' (parseStr2Time "2005-04-03T20:33:31.116-06:00" :: EitherET)
      show zt `shouldBe` "2005-04-04 02:33:31.116 UTC"

    it "parses the string to time with default format" $ do
      let z = fromRight' (parseStr2TimeZone "2005-04-03T20:33:31.116-06:00" :: EitherETZ)
      show z `shouldBe` "-0600"

    it "parses invalid string and throws IllegalTimeFormat Error" $ do
      let zt = fromLeft' (parseStr2Time "2005-04-3T20:33:31.116-06:00" :: EitherET)
      zt `shouldBe` IllegalTimeFormat

    it "parses empty string and throws IllegalTimeFormat Error" $ do
      let zt = fromLeft' (parseStr2Time "" :: EitherET)
      zt `shouldBe` IllegalTimeFormat
