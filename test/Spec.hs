module Main where

import           Tests.DWhat
import           Tests.DWhen
import           Tests.DWhere
import           Tests.DWhy
import           Tests.Object
import           Tests.Parser -- it's not being used right now
import           Tests.Utils

import           Test.Hspec   (hspec)

main :: IO ()
main = do
  -- DWhere
  hspec testReadSGLN
  hspec testPrintSGLN

  -- -- DWhy
  hspec testDisposition

  -- DWhen
  hspec testParseTime
  hspec testMkDWhen
  
  -- Object
  hspec testObjectID
  
  -- Parser
  -- hspec testParser
  
  -- Utils
  hspec testRevertCamelCase
  hspec testMkCamelCase
  hspec testParseURI
  
  -- DWhat
  hspec testBizStep
  hspec testBizTransaction
  hspec testPpDWhat
