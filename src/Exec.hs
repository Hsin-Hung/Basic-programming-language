module Exec where

import Data.Set (Set)
import qualified Data.Set as Set

import Ast
import Eval
import Parser
import ParserMonad



data LangOut = ParseError | RuntimeError String | Ok Val deriving (Show, Eq)

-- | execute the program as a string and get the result
exec :: String -> LangOut
exec s = case (parse parser) s of
           Just (ast,"") -> case run ast of
                              Right v -> Ok v
                              Left e -> RuntimeError e
           _ -> ParseError

runFile path = 
  do program <- readFile path
     return $ exec program

parseFile path = 
  do program <- readFile path
     return $ parse parsemodule program
