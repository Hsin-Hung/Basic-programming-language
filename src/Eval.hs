module Eval where

import Data.Map (Map)
import qualified Data.Map as Map

import Ast
import StateErrorMonad



-- the goal of the program is to return a value, what values are possible?
data Val = I Integer
         | B Bool
         | S String
         deriving Eq

instance Show Val where
  show (I i) = show i
  show (B b) = show b
  show (S s) = show s

-- | helper function that runs with the default environment (for example, the stdLib in week 10)
-- return either the error string or the value, along with everything that was printed
run :: ModuleAst  -- ^ run this Ast
      -> Either String Val --(Either String Val, [String])  
                           -- ^ (error message or result value, all the printings)
run a = evalModule a

type State = Map String Val

type Procs = Map String ProcAst


getDfn :: String -> StateError String (Map String d) s d
getDfn s = do e <- getDfns
              case (Map.lookup s e) of
                Just a  -> return a              
                Nothing -> StateError (\_ _ -> Left "unbound dfn!")

getVar :: String -> StateError String dfn (Map String d) d
getVar v = do e <- getEnv
              case (Map.lookup v e) of
                Just a  -> return a
                Nothing -> StateError (\_ _ -> Left "unbound var!")

setVar :: String -> d -> StateError msg dfn (Map String d) ()
setVar n v = do e <- getEnv
                StateError (\dfn msg -> Right (Map.insert n v e, ()))
                

                
   
evalModule :: ModuleAst -> (Either String Val)
evalModule (ModuleAst n []) = Left "Empty Module!"
evalModule (ModuleAst n (f:fs)) = let (fn,p) = f
                                      et = runStateError (evalProc p []) (Map.fromList fs) Map.empty -- entry point is main
                                  in  case et of
                                        Right (s,a) -> Right a
                                        Left str    -> Left str

-- every procedure call gets a new environment
evalProc :: ProcAst -> [Val] -> StateError String Procs State Val
evalProc (ProcAst strs stmts) vs = local (evalStmt (Seq stmts)) (Map.fromList (zip strs vs))


tloc = runStateError (do setVar "x" (I 3)
                         ans <- local (do setVar "x" (I 5)
                                          return "hi") Map.empty
                         return ans) Map.empty Map.empty
           

-- basic way to eval 
evalStmt' :: Stmt -> StateError String Procs State Val -> StateError String Procs State Val
evalStmt' (Return e) next = evalExpr e
evalStmt' (Assign n e) next = do e' <- evalExpr e
                                 setVar n e'
                                 next
evalStmt' (If b t e) next = do br <- evalBool b
                               case br of 
                                True -> evalStmt' (Seq t) next
                                _    -> evalStmt' (Seq e) next

evalStmt' (While b d) next = do br <- evalBool b
                                case br of
                                  True -> evalStmt' (Seq (d ++ [While b d])) next
                                  _    -> next


evalStmt' (Seq []) next = next
evalStmt' (Seq (x:xs)) next = evalStmt' x (evalStmt' (Seq xs) next)
--evalStmt' (Seq (x:xs)) next = evalStmt' (Seq xs) (evalStmt' x next)


evalStmt :: Stmt -> StateError String Procs State Val
evalStmt stmt = evalStmt' stmt (StateError (\dfn s -> Right (s, I 0)))


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

-- helper functions that take care of type issues

evalInt :: Expr -> StateError String Procs State Integer
evalInt a = do x <- evalExpr a
               case x of 
                I n -> return n
                _   ->  StateError (\_ _ -> Left "Type Error, Expect Integers!")

evalBool :: Expr -> StateError String Procs State Bool
evalBool a = do x <- evalExpr a
                case x of 
                  B b -> return b
                  _   ->  StateError (\_ _ -> Left "Type Error, Expect Booleans!")
               


evalExpr :: Expr -> StateError String Procs State Val
evalExpr (Var str) = getVar str
evalExpr (LiteralInt i) = return (I i)
evalExpr (Plus l r) = do lr <- evalInt l
                         rr <- evalInt r 
                         return (I (lr+rr))
evalExpr (Minus l r) = do lr <- evalInt l
                          rr <- evalInt r 
                          return (I (lr-rr))
evalExpr (Mult l r) = do lr <- evalInt l
                         rr <- evalInt r 
                         return (I (lr*rr))
evalExpr (Div l r) = do lr <- evalInt l
                        rr <- evalInt r 
                        case rr of
                         0 -> StateError (\_ _ -> Left "Div by zero!")
                         _ -> return (I (lr `div` rr))
evalExpr (Mod l r) = do lr <- evalInt l
                        rr <- evalInt r
                        return (I (lr `mod` rr))
evalExpr (Exp l r) = do lr <- evalInt l
                        rr <- evalInt r
                        return (I (lr^rr))

evalExpr (LiteralBool b) = return (B b) 
evalExpr (And l r) = do lr <- evalBool l
                        rr <- evalBool r
                        return (B (lr&&rr))
evalExpr (Or l r) = do lr <- evalBool l
                       rr <- evalBool r
                       return (B (lr||rr))
evalExpr (Not b) = do lr <- evalBool b              
                      return (B (not lr))


evalExpr (Lt l r) = do lr <- evalInt l
                       rr <- evalInt r
                       return (B (lr<rr))
evalExpr (Lte l r) = do lr <- evalInt l
                        rr <- evalInt r
                        return (B (lr<=rr))
evalExpr (Gt l r) = do lr <- evalInt l
                       rr <- evalInt r
                       return (B (lr>rr))
evalExpr (Gte l r) = do lr <- evalInt l
                        rr <- evalInt r
                        return (B (lr>=rr))
evalExpr (Eq l r) = do lr <- evalInt l
                       rr <- evalInt r
                       return (B (lr==rr))
evalExpr (Ne l r) = do lr <- evalInt l
                       rr <- evalInt r
                       return (B (lr/=rr))

evalExpr (Call n exs) = do f <- getDfn n
                           vs <- mapM (evalExpr) exs
                           evalProc f vs
