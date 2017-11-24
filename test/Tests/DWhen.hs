module Tests.DWhen (
--   testParseTime
-- , testMkDWhen
) where

-- import           Data.Either.Combinators
-- import           Data.Time
-- import           Test.Hspec

-- import           Data.GS1.DWhen

-- type EitherET  = Either EPCISTimeError EPCISTime
-- type EitherETZ = Either EPCISTimeError TimeZone

-- testParseTime :: Spec
-- testParseTime =
--   describe "parse string to time" $ do
--     it "parses the string to time with default format" $ do
--       let zt = fromRight' (parseStr2Time "2005-04-03T20:33:31.116-06:00" :: EitherET)
--       show zt `shouldBe` "2005-04-04 02:33:31.116 UTC"

--     it "parses the string to time with default format" $ do
--       let z = fromRight' (parseStr2TimeZone "2005-04-03T20:33:31.116-06:00" :: EitherETZ)
--       show z `shouldBe` "-0600"

--     it "parses invalid string and throws IllegalTimeFormat Error" $ do
--       let zt = fromLeft' (parseStr2Time "2005-04-3T20:33:31.116-06:00" :: EitherET)
--       zt `shouldBe` IllegalTimeFormat

--     it "parses empty string and throws IllegalTimeFormat Error" $ do
--       let zt = fromLeft' (parseStr2Time "" :: EitherET)
--       zt `shouldBe` IllegalTimeFormat

-- testMkDWhen :: Spec
-- testMkDWhen = do
--   describe "create valid DWhen" $ do
--     it "creates DWhen from two valid time Strngs" $ do
--       let et = "2005-04-03T20:33:31.116-06:00"
--       let rt = "2005-04-03T20:39:47.16-06:00"

--       let pet = fromRight' (parseStr2Time et :: Either EPCISTimeError EPCISTime)
--       let prt = fromRight' (parseStr2Time rt :: Either EPCISTimeError EPCISTime)
--       let tz = fromRight' (parseStr2TimeZone et :: Either EPCISTimeError TimeZone)
--       let dwhen = DWhen pet (Just prt) tz

--       mkDWhen et rt `shouldBe` Just dwhen

--     it "event time should be smaller than record time" $ do
--       let et = "2005-04-03T20:39:47.16-06:00"
--       let rt = "2005-04-03T20:33:31.116-06:00"

--       mkDWhen et rt `shouldBe` Nothing

--     it "ignores the record time" $ do
--       let et = "2005-04-03T20:33:31.116-06:00"
--       let rt = "2005-04-03T20:39:47.16-06:00-invalid-format"

--       let pet = fromRight' (parseStr2Time et :: Either EPCISTimeError EPCISTime)
--       let tz = fromRight' (parseStr2TimeZone et :: Either EPCISTimeError TimeZone)
--       let dwhen = DWhen pet Nothing tz

--       mkDWhen et rt `shouldBe` Just dwhen

--     it "does not ignore invalid event time" $ do
--       let et = "2005-04-3T20:39:47.16-06:00-invalid-format"
--       let rt = "2005-04-3T20:33:31.116-06:00"

--       mkDWhen et rt `shouldBe` Nothing

--   describe "create valid DWhen by mkDWhen'" $ do
--     it "creates valid DWhen from one time string" $ do
--       let t = "2017-01-24T13:08:24.11+10:00"
--       let pt = fromRight' (parseStr2Time t :: Either EPCISTimeError EPCISTime)
--       let tz = fromRight' (parseStr2TimeZone t :: Either EPCISTimeError TimeZone)

--       let dwhen = DWhen pt (Just pt) tz

--       mkDWhen' t `shouldBe` Just dwhen

--     it "fails on invalid string" $ do
--       let t = "invalid input"
--       mkDWhen' t `shouldBe` Nothing
