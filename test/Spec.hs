module Main where

import           Tests.DWhat
import           Tests.DWhen
import           Tests.DWhere
import           Tests.DWhy
import           Tests.Object
import           Tests.Parser
import           Tests.Utils

import           Test.Hspec   (hspec)

main :: IO ()
main = do
  -- DWhere
  hspec testSGLN
  -- hspec testMkEPC

  -- -- DWhy
  -- hspec testBizStep
  -- hspec testDisposition

  -- -- DWhat
  -- hspec testBizTransaction
  -- hspec testMkDWhat

  -- -- DWhen
  -- hspec testParseTime
  -- hspec testMkDWhen

  -- -- Object
  -- hspec testObjectID

  -- -- Parser
  -- hspec testParser

  -- -- Utils
  hspec testRevertCamelCase
  hspec testMkCamelCase
