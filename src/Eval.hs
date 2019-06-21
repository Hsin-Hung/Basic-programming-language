module Eval where

import Data.Map (Map)
import qualified Data.Map as Map

import Ast
import StateErrorMonad



-- the goal of the program is to return a value, what values are possible?
data Val -- = ... deriving Show

-- | helper function that runs with the default environment (for example, the stdLib in week 10)
-- return either the error string or the value, along with everything that was printed
run :: ModuleAst  -- ^ run this Ast
      -> (Either String Val, [String])  -- ^ (error message or result value, all the printings)
run a = undefined 


type State = Map String Val

type Procs = Map String ProcAst

getDfn :: String -> StateError msg (Map String d) s d
getDfn s = undefined

getVar :: String -> StateError msg dfn (Map String d) d
getVar v = undefined

setVar :: String -> d -> StateError msg dfn (Map String d) ()
setVar n v = undefined

	 
evalModule :: ModuleAst -> (Either String Val)
evalModule = undefined -- build up the definitions of module and eval the main procedure


-- every procedure call gets a new environment
evalProc :: ProcAst -> [Val] -> StateError String Procs State  Val
evalProc = undefined

-- basic way to eval 
evalStmt :: Stmt -> StateError String Procs State Val
evalStmt = undefined

{-
-- unfortunately we can't beak out on the first return!
exBad = runStateError (evalStmt $  Return (LitteralInt 2) `Seperator` Return (LitteralInt 7) ) Map.empty Map.empty

alternatively you may want to evaluate statements like this
evalStmt' :: Stmt -> (StateError String Procs State Val) -> StateError String Procs State Val
evalStmt' (Seperator l r) next = evalStmt' l $ evalStmt' r next
evalStmt' (Return e) next = evalExpr e -- opt to skip the next stuff if you finish early
evalStmt' (Assign n e) next = do
  e' <- evalExpr e
  setVar n e'
  next
...
it uses a simplified version of "continuation passing style", allows much finer control over control flow

evalStmt :: Stmt -> StateError String Procs State Val
evalStmt stmt = evalStmt' stmt (pure undefined) 
  
-- we can't beak out on the first return!
exGood = runStateError (evalStmt  $ Return (LitteralInt 2) `Seperator` Return (LitteralInt 7) ) Map.empty Map.empty
exGood1 = runStateError (evalStmt $ ("x" `Assign` (LitteralInt 2)) `Seperator` ("x" `Assign` (LitteralInt 7)) ) Map.empty Map.empty 
-}



-- 


evalExpr :: Expr -> StateError String Procs State  Val
evalExpr = undefined
  