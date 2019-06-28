module EvalTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 
import Data.Map (Map)
import qualified Data.Map as Map
import Ast
import Eval
import Exec
import StateErrorMonad


isError :: LangOut -> Bool 
isError (RuntimeError _) = True
isError _ = False

-- provide tests that show your run/eval works
zero = (LiteralInt 0)
one = (LiteralInt 1)
none = (LiteralInt (-1))
two = (LiteralInt 2)
ntwo = (LiteralInt (-2))
three = (LiteralInt 3)
nthree = (LiteralInt (-3))
four = (LiteralInt 4)
nfour = (LiteralInt (-4))
five = (LiteralInt 5)
nfive = (LiteralInt (-5))
six = (LiteralInt 6)
seven = (LiteralInt 7)
nseven = (LiteralInt (-7))
ten = (LiteralInt 10)
twelve = (LiteralInt 12)

true = (LiteralBool True)
false = (LiteralBool False)

runExpr :: Expr -> LangOut
runExpr e = case (runStateError (evalExpr e) Map.empty Map.empty) of
              Right (_,v) -> Ok v
              Left e -> RuntimeError e


runStmt :: Stmt -> LangOut
runStmt s = case (runStateError (evalStmt s) Map.empty Map.empty) of
              Right (_,v) -> Ok v
              Left e -> RuntimeError e

runMod :: ModuleAst -> LangOut
runMod p = case (evalModule p) of
              Right v -> Ok v
              Left e -> RuntimeError e


tests = testGroup "EvalTest" 
  [
    testCase "Basic Arithmetic" $
        do 
          assertEqual "2 + 4 =? " (Ok $ I 6) (runExpr $ two `Plus` four)
          assertEqual "2 + -1 =? " (Ok $ I 1) (runExpr $ two `Plus` none)
          assertEqual "2 - 4 =? " (Ok $ I (-2)) (runExpr $ two `Minus` four)
          assertEqual "2 - (-4) =? " (Ok $ I 6) (runExpr $ two `Minus` nfour)
          assertEqual "3 * 2 =? " (Ok $ I 6) (runExpr $ three `Mult` two)
          assertEqual "2 * -2 =? " (Ok $ I (-4)) (runExpr $ two `Mult` ntwo)
          assertEqual "7 / 2 =? " (Ok $ I (3)) (runExpr $ seven `Div` two)
          assertEqual "4 / -2 =? " (Ok $ I (-2)) (runExpr $ four `Div` ntwo)
          assertEqual "10 % 2 =? " (Ok $ I 0) (runExpr $ ten `Mod` two)
          assertEqual "-5 % 12 =? " (Ok $ I 7) (runExpr $ nfive `Mod` twelve)
          assertEqual "3 ^ 2 =? " (Ok $ I 9) (runExpr $ three `Exp` two)
          assertEqual "-7 ^ 2 =? " (Ok $ I 49) (runExpr $ nseven `Exp` two),
          
    testCase "division by 0" $
        assertBool "1/0" $ isError (runExpr $ one `Div` zero),
          
    testCase "Compound Arithmetic" $
        do 
          assertEqual "2 + 4 * 3 =? " (Ok $ I 14) (runExpr $ two `Plus` (four `Mult` three))
          assertEqual "(2 + -4) * 3 =? " (Ok $ I (-6)) (runExpr $ (two `Plus` nfour) `Mult` three)
          assertEqual "2 * 3 + 3 * 2 - 4 =? " (Ok $ I 8) (runExpr $ ((two `Mult` three) `Plus` (three `Mult` two)) `Minus` four)
          assertEqual "2 * (3 + 3) * (2 - 4) =? " (Ok $ I (-24)) (runExpr $ (two `Mult` (three `Plus` three)) `Mult` (two `Minus` four))
          assertEqual "12 / (3 + 3) * (2 - 4) =? " (Ok $ I (-4)) (runExpr $ (twelve `Div` (three `Plus` three)) `Mult` (two `Minus` four))
          assertEqual "3 * 4 % 3 + 4 ^ 2 =? " (Ok $ I (16)) (runExpr $ ((three `Mult` four) `Mod` three) `Plus` (four `Exp` two))
          assertEqual "(3 * 4) % (3 + 4) ^ 2 =? " (Ok $ I (12)) (runExpr $ ((three `Mult` four) `Mod` ((three `Plus` four) `Exp` two))),

    testCase "Basic Boolean" $
        do 
          assertEqual "true && true =? " (Ok $ B True) (runExpr $ true `And` true)
          assertEqual "true && false =? " (Ok $ B False) (runExpr $ true `And` false)
          assertEqual "true || false =? " (Ok $ B True) (runExpr $ true `Or` false)
          assertEqual "false || false =? " (Ok $ B False) (runExpr $ false `Or` false)
          assertEqual "!true =? " (Ok $ B False) (runExpr $ Not true),

    testCase "Basic Comparison" $ 
        do 
          assertEqual "1 == 1 =? " (Ok $ B True) (runExpr $ one `Eq` one)
          assertEqual "1 == 2 =? " (Ok $ B False) (runExpr $ one `Eq` two)
          assertEqual "1 /= 1 =? " (Ok $ B False) (runExpr $ one `Ne` one)
          assertEqual "1 /= 2 =? " (Ok $ B True) (runExpr $ one `Ne` two)
          assertEqual "1 < 1 =? " (Ok $ B False) (runExpr $ one `Lt` one)
          assertEqual "1 <= 1 =? " (Ok $ B True) (runExpr $ one `Lte` one)
          assertEqual "1 > 2 =? " (Ok $ B False) (runExpr $ one `Gt` two)
          assertEqual "1 >= 2 =? " (Ok $ B False) (runExpr $ one `Gte` two),

    testCase "If Statements" $
        do 
          assertEqual "if true {4} else {2} =? " (Ok $ I 4)  (runStmt $ If true [Return four] [Return two])
          assertEqual "if true && false {1} else {4} =? " (Ok $ I 4)  (runStmt $ If (true `And` false)  [Return one] [Return four])
          assertEqual "if 3 < 0 {1} else {2}  =? " (Ok $ I 2)  (runStmt $ If (three `Lt` zero)  [Return one] [Return two])
          assertEqual "if 3 > 2 {1} else {2}  =? " (Ok $ I 1)  (runStmt $ If (three `Gt` two)  [Return one] [Return two]),

    testCase "Assign Statements" $
        do 
          assertEqual "x := 5; return x" (Ok $ I 5) (runStmt $ Seq [Assign "x" five, Return (Var "x")])
          assertEqual "x := 5;x := 6;return x" (Ok $ I 6) (runStmt $ Seq [Assign "x" five, Assign "x" six, Return (Var "x")])
          assertEqual "x := 5;y := 6 return x+y" (Ok $ I 11) (runStmt $ Seq [Assign "x" five, Assign "y" six, Return ((Var "x") `Plus` (Var "y"))])
          assertEqual "x := true;y := false return x&&y" (Ok $ B False) (runStmt $ Seq [Assign "x" true, Assign "y" false, Return ((Var "x") `And` (Var "y"))]),

    testCase "While loops" $
        do 
          assertEqual "x := 0; while(x<5) { x := x + 1}; return x" (Ok $ I 5) (runStmt $ Seq [Assign "x" zero, While ((Var "x") `Lt` five) [Assign "x" ((Var "x") `Plus` one)], Return (Var "x")])
          assertEqual "x := 0;y := 0; while(x<5) { y := y + x; x:= x + 1}; return y" (Ok $ I 10) (runStmt $ Seq [Assign "x" zero, Assign "y" zero, While ((Var "x") `Lt` five) [Assign "y" ((Var "y") `Plus` (Var "x")),Assign "x" ((Var "x") `Plus` one)], Return (Var "y")]),

    testCase "Basic function Calls" $
        do 
          assertEqual "def y(){return 5}/ call y()" (Ok $ I 5) (runMod $ ModuleAst "main" [("x",ProcAst [] [Return $ Call "y" []]),("y",ProcAst [] [Return five])])
          assertEqual "def y(n){return n}/ call y(10)" (Ok $ I 10) (runMod $ ModuleAst "main" [("x",ProcAst [] [Return $ Call "y" [ten]]),("y",ProcAst ["n"] [Return (Var "n")])])
          assertEqual "def y(a,b,c){return a+b+c}/ call y(1,2,3)" (Ok $ I 6) (runMod $ ModuleAst "main" [("x",ProcAst [] [Return $ Call "y" [one,two,three]]),("y",ProcAst ["a","b","c"] [Return (((Var "a") `Plus` (Var "b")) `Plus` (Var "c"))])])
          assertEqual "def z(n){return n};def y(n){return n}/return z(1)+y(2)" (Ok $ I 3) (runMod $ ModuleAst "main" [("x",ProcAst [] [Return $ (Call "z" [one]) `Plus` (Call "y" [two])]),("z",ProcAst ["n"] [Return $ Var "n"]), ("y",ProcAst ["n"] [Return $ Var "n"])]),

    testCase "Function Call Local Environments" $
        do 
          assertEqual "def y(){x := 5;return x}/ x:= 10; y:= call y(); return x" (Ok $ I 10) (runMod $ ModuleAst "main" [("x",ProcAst [] [Assign "x" ten,Assign "y" (Call "y" []),Return $ Var "x"]),("y",ProcAst [] [Assign "x" five,Return $ Var "x"])])
          assertEqual "def z(n){return n};def y(n){return n}/return z(1)+y(2)" (Ok $ I 3) (runMod $ ModuleAst "main" [("x",ProcAst [] [Return $ (Call "z" [one]) `Plus` (Call "y" [two])]),("z",ProcAst ["n"] [Return $ Var "n"]), ("y",ProcAst ["n"] [Return $ Var "n"])])
          assertBool  "def y(){x := 5;return x}/ y:= call y(); return x" $ isError (runMod $ ModuleAst "main" [("x",ProcAst [] [Assign "y" (Call "y" []), Return $ Var "x"]),("y",ProcAst [] [Assign "x" five,Return $ Var "x"])])



  ]

