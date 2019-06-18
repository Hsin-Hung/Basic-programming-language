module ParserTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck 

import Ast
import Parser

-- provide tests that show your parser works

tests = testGroup "ParserTest" 
  [
  error "no tests yet!"
  -- ...
  ]

instance Arbitrary Ast where
    arbitrary = sized undefined
    shrink = undefined
