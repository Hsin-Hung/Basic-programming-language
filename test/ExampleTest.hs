module ExampleTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)


import Exec
import Eval

-- provide tests that show your run/eval works

tests = testGroup "ExampleTest" 
  [
  testCase  "Euler problem 1" $ do res <- runFile "example/euler1.txt"
                                   assertEqual "" (Ok $ I 233168) $  res,
  testCase  "Euler problem 2" $ do res <- runFile "example/euler2.txt"
                                   assertEqual "" (Ok $ I 4613732) $  res,
  testCase  "Euler problem 3" $ do res <- runFile "example/euler3.txt"
                                   assertEqual "" (Ok $ I 6857) $  res,
  testCase  "Euler problem 5" $ do res <- runFile "example/euler5.txt"
                                   assertEqual "" (Ok $ I 232792560) $  res,
  testCase  "Euler problem 6" $ do res <- runFile "example/euler6.txt"
                                   assertEqual "" (Ok $ I 25164150) $  res,
  testCase  "Euler problem 9" $ do res <- runFile "example/euler9.txt"
                                   assertEqual "" (Ok $ I 31875000) $  res           
  
  ]

