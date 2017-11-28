module Main where

import           Tests.DWhat
import           Tests.DWhen
import           Tests.DWhere
import           Tests.DWhy
import           Tests.Object
--import           Tests.Parser -- DELETEME
import           Tests.Utils

import           Test.Hspec   (hspec)

main :: IO ()
main = do
  -- DWhere
  hspec testReadSGLN
  hspec testPrintSGLN

  -- -- DWhy
  hspec testBizStep
  hspec testDisposition

  
  -- DWhen
  hspec testParseTime
  hspec testMkDWhen
  
  -- Object
  hspec testObjectID
  
  -- Parser
  -- hspec testParser -- DELETEME
  
  -- Utils
  hspec testRevertCamelCase
  hspec testMkCamelCase
  hspec testParseURI
  
  -- DWhat
  hspec testBizTransaction
  hspec testMkDWhat