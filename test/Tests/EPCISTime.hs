module Tests.EPCISTime where

import Control.Exception (evaluate)
import Test.Hspec
import Data.GS1.EPCISTime

testTime :: Spec
testTime = do
  describe "EPCISTime" $ do
    it "Date accepts valid input" $
      (show (date 2016 2 29)) `shouldBe` "2016-02-29"

    it "Date accpets valid input" $
      (show (date 1999 12 3)) `shouldBe` "1999-12-03"

    it "Date throws Exception on invalid input" $
      evaluate (date 2015 2 29) `shouldThrow` anyException

    it "Date throws Exception on invalid input" $
      evaluate (date 2015 4 31) `shouldThrow` anyException

    it "Time accepts valid input" $
      (show (time 6 2 3)) `shouldBe` "06:02:03"

    it "Time accepts valid input" $
      (show (time 21 12 37)) `shouldBe` "21:12:37"

    it "Time throws Exception on invalid input" $
      (evaluate (time 23 66 23 )) `shouldThrow` anyException

    it "Time throws Exception on invalid input" $
      (evaluate (time 24 46 00 )) `shouldThrow` anyException

