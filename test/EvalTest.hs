module EvalTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 

import Ast
import Eval
import Exec


isError :: LangOut -> Bool 
isError (RuntimeError _) = True
isError _ = False

-- provide tests that show your run/eval works

tests = testGroup "EvalTest" 
  [
    testCase "Basic Arithmetic" $
        do 
          assertEqual "2 + 4 =? " (Ok $ I 6) (exec "module test{def test(){return 2+4}}")
          assertEqual "2 + -1 =? " (Ok $ I 1) (exec "module test{def test(){return 2+(-1)}}")
          assertEqual "2 - 4 =? " (Ok $ I (-2)) (exec "module test{def test(){return 2-4}}")
          assertEqual "2 - (-4) =? " (Ok $ I 6) (exec "module test{def test(){return 2-(-4)}}")
          assertEqual "3 * 2 =? " (Ok $ I 6) (exec "module test{def test(){return 3*2}}")
          assertEqual "2 * -2 =? " (Ok $ I (-4)) (exec "module test{def test(){return 2*(-2)}}")
          assertEqual "7 / 2 =? " (Ok $ I (3)) (exec "module test{def test(){return 7/2}}")
          assertEqual "4 / -2 =? " (Ok $ I (-2)) (exec "module test{def test(){return 4/(-2)}}")
          assertEqual "10 % 2 =? " (Ok $ I 0) (exec "module test{def test(){return 10%2}}")
          assertEqual "-5 % 12 =? " (Ok $ I 7) (exec "module test{def test(){return -5%12}}")
          assertEqual "3 ^ 2 =? " (Ok $ I 9) (exec "module test{def test(){return 3^2}}")
          assertEqual "-7 ^ 2 =? " (Ok $ I 49) (exec "module test{def test(){return -7^2}}"),
          
    testCase "division by 0" $
        assertBool "1/0" $ isError (exec "module test{def test(){return 1/0}}"),
          
    testCase "Compound Arithmetic" $
        do 
          assertEqual "2 + 4 * 3 =? " (Ok $ I 14) (exec "module test{def test(){return 2+4*3}}")
          assertEqual "(2 + -4) * 3 =? " (Ok $ I (-6)) (exec "module test{def test(){return (2+(-4))*3}}")
          assertEqual "2 * 3 + 3 * 2 - 4 =? " (Ok $ I 8) (exec "module test{def test(){return 2*3+3*2-4}}")
          assertEqual "2 * (3 + 3) * (2 - 4) =? " (Ok $ I (-24)) (exec "module test{def test(){return 2*(3+3)*(2-4)}}")
          assertEqual "12 / (3 + 3) * (2 - 4) =? " (Ok $ I (-4)) (exec "module test{def test(){return 12/(3+3)*(2-4)}}")
          assertEqual "3 * 4 % 3 + 4 ^ 2 =? " (Ok $ I (16)) (exec "module test{def test(){return 3*4%3+4^2}}")
          assertEqual "(3 * 4) % (3 + 4) ^ 2 =? " (Ok $ I (12)) (exec "module test{def test(){return (3*4)%(3+4)^2}}"),

    testCase "Basic Boolean" $
        do 
          assertEqual "true && true =? " (Ok $ B True) (exec "module test{def test(){return true&&true}}")
          assertEqual "true && false =? " (Ok $ B False) (exec "module test{def test(){return true&&false}}")
          assertEqual "true || false =? " (Ok $ B True) (exec "module test{def test(){return true||false}}")
          assertEqual "false || false =? " (Ok $ B False) (exec "module test{def test(){return false&&false}}")
          assertEqual "!true =? " (Ok $ B False) (exec "module test{def test(){return !true}}"),

    testCase "Basic Comparison" $ 
        do 
          assertEqual "1 == 1 =? " (Ok $ B True) (exec "module test{def test(){return 1==1}}")
          assertEqual "1 == 2 =? " (Ok $ B False) (exec "module test{def test(){return 1==2}}")
          assertEqual "1 /= 1 =? " (Ok $ B False) (exec "module test{def test(){return 1/=1}}")
          assertEqual "1 /= 2 =? " (Ok $ B True) (exec "module test{def test(){return 1/=2}}")
          assertEqual "1 < 1 =? " (Ok $ B False) (exec "module test{def test(){return 1<1}}")
          assertEqual "1 <= 1 =? " (Ok $ B True) (exec "module test{def test(){return 1<=1}}")
          assertEqual "1 > 2 =? " (Ok $ B False) (exec "module test{def test(){return 1>2}}")
          assertEqual "1 >= 2 =? " (Ok $ B False) (exec "module test{def test(){return 1>=2}}"),

    testCase "If Statements" $
        do 
          assertEqual "if true {4} else {2} =? " (Ok $ I 4)  (exec "module test{def test(){if true {return 4} else {return 2}}}")
          assertEqual "if true && false {1} else {4} =? " (Ok $ I 4)  (exec "module test{def test(){if true && false {return 1} else {return 4}}}")
          assertEqual "if 3 < 0 {1} else {2}  =? " (Ok $ I 2)  (exec "module test{def test(){if 3<0 {return 1} else {return 2}}}")
          assertEqual "if 3 > 2 {1} else {2}  =? " (Ok $ I 1)  (exec "module test{def test(){if 3>2 {return 1} else {return 2}}}"),

    testCase "Assign Statements" $
        do 
          assertEqual "x := 5; return x" (Ok $ I 5) (exec "module test{def test(){x:=5;return x}}")
          assertEqual "x := 5;x := 6;return x" (Ok $ I 6) (exec "module test{def test(){x:=5;x:=6;return x}}")
          assertEqual "x := 5;y := 6 return x+y" (Ok $ I 11) (exec "module test{def test(){x:=5;y:=6;return x+y}}")
          assertEqual "x := true;y := false return x&&y" (Ok $ B False) (exec "module test{def test(){x:=true;y:=false;return x&&y}}"),

    testCase "While loops" $
        do 
          assertEqual "x := 0; while(x<5) { x := x + 1}; return x" (Ok $ I 5) (exec "module test{def test(){x:=0; while (x<5){x:=x+1};return x}}")
          assertEqual "x := 0;y := 0; while(x<5) { y := y + x; x:= x + 1}; return y" (Ok $ I 10) (exec "module test{def test(){x:=0;y:=0; while (x<5){y:=x+y;x:=x+1};return y}}"),

    testCase "Basic function Calls" $
        do 
          assertEqual "def y(){return 5}/ call y()" (Ok $ I 5) (exec "module test{def x(){return y()};def y(){return 5}}")
          assertEqual "def y(n){return n}/ call y(10)" (Ok $ I 10) (exec "module test{def x(){return y(10)};def y(n){return n}}")
          assertEqual "def y(a,b,c){return a+b+c}/ call y(1,2,3)" (Ok $ I 6) (exec "module test{def x(){return y(1,2,3)};def y(a,b,c){return a+b+c}}")
          assertEqual "def z(n){return n};def y(n){return n}/return z(1)+y(2)" (Ok $ I 3) (exec "module test{def x(){return z(1)+y(2)};def y(n){return n};def z(n){return n}}"),

    testCase "Function Call Local Environments" $
        do 
          assertEqual "def y(){x := 5;return x}/ x:= 10; y:= call y(); return x" (Ok $ I 10) (exec "module test{def x(){x:=10;y:=y();return x};def y(){x:=5;return x}}")
          assertEqual "def z(n){return n};def y(n){return n}/return z(1)+y(2)" (Ok $ I 3) (exec "module test{def x(){return z(1)+y(2)};def y(n){return n};def z(n){return n}}")
          assertBool  "def y(){x := 5;return x}/ y:= call y(); return x" $ isError (exec "module test{def x(){y:=y();return x};def y(){x:=5;return x}}")



  ]

