module ParserTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck 

import Ast
import ParserMonad (parse)
import Parser (parseExp,parseStmt,parseProcedureBod,parsemodule)

-- provide tests that show your parser works

tests = testGroup "ParserTest" 
  [
   testProperty "parse should return the same EXPR when fully parenthesized" $
                ((\ x -> Just (x , "") == (parse parseExp $ showFullyParenExpr x)) :: Expr -> Bool),

   testProperty "parse should return the same EXPR when pretty printed" $
                ((\ x -> Just (x , "") == (parse parseExp $ showPrettyExpr x 0)) :: Expr -> Bool),

   testProperty "parse should return the same STMT when fully parenthesized" $
                ((\ x -> Just (x , "") == (parse parseStmt $ showFullyParenStmt x)) :: Stmt -> Bool),

   testProperty "parse should return the same STMT when pretty printed" $
                ((\ x -> Just (x , "") == (parse parseStmt $ showPrettyStmt x 0)) :: Stmt -> Bool)

  ]
    
instance Arbitrary Stmt where
    arbitrary = sized arbitrarySizedStmt
    
    shrink (Assign x e) = [Assign x e' | e' <- shrink e] 
    shrink (Return e) = [Return e' | e' <- shrink e]
    shrink (If e s1 s2) = s1 ++ s2 ++ [If e s1' s2' | (s1', s2') <- shrink (s1, s2)]
    shrink (While e s1) = s1 ++ [While e s1' | s1' <- shrink s1]
    shrink _ = []
    
arbitrarySizedStmt :: Int -> Gen Stmt
arbitrarySizedStmt m | m < 1 = do e <- arbitrarySizedExpr m
                                  x <- elements ["x","y","z"]
                                  node <- elements [Assign x e, Return e]
                                  return $ node

arbitrarySizedStmt m | otherwise = do 
                                      ifStmt <- arbitrarySizedIf m
                                      whileStmt <- arbitrarySizedWhile m
                                      node <- elements [ifStmt,whileStmt]
                                      return $ node



arbitrarySizedIf ::  Int -> Gen Stmt
arbitrarySizedIf m = do x <- arbitrarySizedExpr (m `div` 3)
                        y <- arbitrarySizedStmt (m `div` 3)
                        z <- arbitrarySizedStmt (m `div` 3)
                        return $ If x [y] [z]

arbitrarySizedWhile ::  Int -> Gen Stmt
arbitrarySizedWhile m = do x <- arbitrarySizedExpr (m `div` 2)
                           y <- arbitrarySizedStmt (m `div` 2)
                           return $ While x [y]

instance Arbitrary Expr where
    arbitrary = sized arbitrarySizedExpr
    shrink (LiteralInt i) = [LiteralInt i' | i' <-  shrink i]
    shrink (LiteralBool i) = [LiteralBool i' | i' <-  shrink i]
    shrink (And l r) = [l, r] ++ [And l' r' | (l', r') <-  shrink (l, r)]
    shrink (Or l r) = [l, r] ++ [Or l' r' | (l', r') <-  shrink (l, r)]
    shrink (Not i) = [i] ++ [Not i' | i' <-  shrink i]
  
    shrink (Plus l r) = [l, r] ++ [Plus l' r' | (l', r') <-  shrink (l, r)]
    shrink (Minus l r) = [l, r] ++ [Minus l' r' | (l', r') <-  shrink (l, r)]
    shrink (Mult l r) = [l, r] ++ [Mult l' r' | (l', r') <-  shrink (l, r)]
    shrink (Div l r) = [l, r] ++ [Div l' r' | (l', r') <-  shrink (l, r)]
    shrink (Mod l r) = [l, r] ++ [Mod l' r' | (l', r') <-  shrink (l, r)]
    shrink (Exp l r) = [l, r] ++ [Exp l' r' | (l', r') <-  shrink (l, r)]

    shrink (Lt l r) = [l, r] ++ [Lt l' r' | (l', r') <-  shrink (l, r)]
    shrink (Lte l r) = [l, r] ++ [Lte l' r' | (l', r') <-  shrink (l, r)]
    shrink (Gt l r) = [l, r] ++ [Gt l' r' | (l', r') <-  shrink (l, r)]
    shrink (Gte l r) = [l, r] ++ [Gte l' r' | (l', r') <-  shrink (l, r)]
    shrink (Eq l r) = [l, r] ++ [Eq l' r' | (l', r') <-  shrink (l, r)]
    shrink (Ne l r) = [l, r] ++ [Ne l' r' | (l', r') <-  shrink (l, r)]
  
    shrink _ = []


arbitrarySizedExpr :: Int -> Gen Expr
arbitrarySizedExpr m | m < 1 = do i <- arbitrary
                                  b <- arbitrary
                                  x <- elements ["x", "y", "z"]
                                  node <- elements [Var x, LiteralInt i, LiteralBool b]
                                  return $ node

arbitrarySizedExpr m | otherwise = do l <- arbitrarySizedExpr (m `div` 2)
                                      r <- arbitrarySizedExpr (m `div` 2)
                                      x <- elements ["x", "y", "z"]
                                      node <- elements [Plus l r,
                                                        Minus l r,
                                                        Mult l r,
                                                        Div l r,
                                                        Mod l r,
                                                        Exp l r,
                                                        Lt l r,
                                                        Lte l r,
                                                        Gt l r,
                                                        Gte l r,
                                                        Eq l r,
                                                        Ne l r,
                                                        And l r,
                                                        Or l r,
                                                        Not l
                                                       ]
                                      return node
