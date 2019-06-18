module Main where

import Test.Tasty

import EvalTest
import ParserTest

main = defaultMain testSuite


testSuite =
  testGroup
    "allTests"
    [
    EvalTest.tests,
    ParserTest.tests
    -- ...
    ]
