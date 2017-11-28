{-# LANGUAGE DeriveGeneric         #-}


module Tests.Utils where

import           Data.GS1.Utils
import           Test.Hspec
import           Data.GS1.EPC
import           GHC.Generics

data TestDataType = Foo
                  | Bar
                  | Jaja
                  deriving (Show, Eq, Generic, Read)
{-
$(deriveJSON defaultOptions ''TestDataType)
instance ToSchema TestDataType

makeClassyPrisms ''TestDataType
-}
testDataTypeURI :: String
testDataTypeURI = "blah:foo:bar"

{-
instance URI TestDataType where
  printURI t = testDataTypeURI ++ (revertCamelCase . show t)
  readURI s = let uri = testDataTypeURI in
              parseURI s uri :: Maybe TestDataType
-}

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

testParseURI :: Spec
testParseURI =
  describe "parse a URI" $
    it "parse TestDataType" $
      parseURI (testDataTypeURI++":foo") testDataTypeURI `shouldBe` (Just Foo)
