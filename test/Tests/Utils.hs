module Tests.Utils where

import           Data.GS1.Utils
import           Data.Maybe
import           Test.Hspec

testRevertCamelCase :: Spec
testRevertCamelCase =
  describe "revert camel case to underscore separated" $ do
    it "reverts camel case string starting with upperase char" $
      revertCamelCase "HelloWorld" `shouldBe` "hello_world"

    it "reverts camel case string starting with lowercase char" $
      revertCamelCase "helloWorld" `shouldBe` "hello_world"

    it "does nothing on a string with all lower case char" $
      revertCamelCase "helloworld" `shouldBe` "helloworld"

    it "does nothing on an empty string" $
      revertCamelCase "" `shouldBe` ""

    it "does not insert underscore to a single string starting with upper case char" $
      revertCamelCase "Hello" `shouldBe` "hello"

    it "seperates every char of a string with all uppercase chars" $
      revertCamelCase "HELLO" `shouldBe` "h_e_l_l_o"

testMkCamelCase :: Spec
testMkCamelCase =
  describe "make camel case from underscore separated string" $ do
    it "make camel case string" $
      mkCamelCase "hello_world" `shouldBe` "HelloWorld"

    it "make camel case strign with string containing upper case char" $
      mkCamelCase "hello_World" `shouldBe` "HelloWorld"

testParseTime :: Spec
testParseTime =
  describe "parse string to time" $ do
    it "parses the string to time with default format" $ do
      let zt = fromJust $ parseStr2Time "2005-04-03T20:33:31.116-06:00"
      show zt `shouldBe` "2005-04-04 02:33:31.116 UTC"

    it "parses the string to time with default format" $ do
      let z = fromJust $ parseStr2TimeZone "2005-04-03T20:33:31.116-06:00"
      show z `shouldBe` "-0600"

    it "parses invalid string to Nothing" $
      parseStr2Time "2005-04-3T20:33:31.116-06:00" `shouldBe` Nothing

    it "parses empty string to Nothing" $
      parseStr2Time "" `shouldBe` Nothing
