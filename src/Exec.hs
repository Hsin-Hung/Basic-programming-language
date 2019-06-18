module Exec where

import Data.Set (Set)
import qualified Data.Set as Set

import Ast
import Eval
import Parser
import ParserMonad



data LangOut -- = ...


-- | execute the program as a string and get the result
exec :: String -> LangOut
exec s = undefined

runFile path = 
  do program <- readFile path
     return $ exec program

parseFile path = 
  do program <- readFile path
     return $ parse parsemodule program
