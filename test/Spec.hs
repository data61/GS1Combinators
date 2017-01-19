module Main where

import           Tests.Event
import           Tests.Location
import           Tests.Object
import           Tests.Utils

import           Test.Hspec     (hspec)

main :: IO ()
main = do
  hspec testPassGLN

  hspec testBizStep
  hspec testParseBizStep
  hspec testDisposition
  hspec testBizTransaction
  hspec testCreateDWhat

  hspec testRevertCamelCase
  hspec testMkCamelCase

  hspec testObjectID
