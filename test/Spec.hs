module Main where

import           Tests.Event
import           Tests.Location
import           Tests.Object
import           Tests.Utils

import           Test.Hspec     (hspec)

main :: IO ()
main = do

  hspec testPassGLN
  hspec testMkEPC

  hspec testBizStep
  hspec testDisposition
  hspec testBizTransaction
  hspec testMkDWhat
  hspec testMkDWhen

  hspec testRevertCamelCase
  hspec testMkCamelCase
  hspec testParseTime

  hspec testObjectID
