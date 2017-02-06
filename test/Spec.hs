module Main where

import           Tests.DWhen
import           Tests.DWhere
import           Tests.Event
import           Tests.Object
import           Tests.Parser
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

  hspec testRevertCamelCase
  hspec testMkCamelCase

  hspec testParseTime
  hspec testMkDWhen

  hspec testObjectID

  hspec testParser
