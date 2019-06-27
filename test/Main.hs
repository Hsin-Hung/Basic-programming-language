module Main where

import System.Environment
import Test.Tasty

import EvalTest
import ParserTest
import ExampleTest

main = do setEnv "TASTY_TIMEOUT" "60s"
          setEnv "TASTY_QUICKCHECK_TESTS" "1000"
          setEnv "TASTY_QUICKCHECK_MAX_SIZE" "50"
          defaultMain testSuite
          unsetEnv "TASTY_TIMEOUT"
          unsetEnv "TASTY_QUICKCHECK_TESTS"
          unsetEnv "TASTY_QUICKCHECK_MAX_SIZE"

          


testSuite =
  testGroup
    "allTests"
    [
    EvalTest.tests,
    ParserTest.tests,
    ExampleTest.tests
    ]
