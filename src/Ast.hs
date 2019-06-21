module Ast where

import HelpShow

type Ast = ModuleAst

-- procedure, module name and procure names with their definitions
data ModuleAst = ModuleAst String [(String, ProcAst)] -- assume the program entry point is "main" TODO: better with Map? add imports
  deriving (Eq,Show)

-- procedure, parameter names and a body
data ProcAst = ProcAst [String] [Stmt]  deriving (Eq,Show)

data Stmt = 
      Assign String Expr 
    | If Expr [Stmt] [Stmt]
    | While Expr [Stmt]
    | Return Expr
    deriving Eq
          
{-should include: Assignment, If, While loops, Return 
and perhaps: Separators, ForEach loops
and if you're fancy Print
-}
   
data Expr =
      Var String
    | LiteralInt Integer
    | Plus Expr Expr | Minus Expr Expr | Mult Expr Expr | Div Expr Expr

    | LiteralBool Bool
    | And Expr Expr | Or Expr Expr | Not Expr

    | Lt Expr Expr | Lte Expr Expr | Gt Expr Expr | Gte Expr Expr | Eq Expr Expr | Ne Expr Expr
    deriving Eq

{-should include: 
Vars
procedure calls 
LitteralInt, all the int operations
LitteralBool, all the bool operations
comparisons: <, <=, ==, !=

and perhaps: Nil, Cons and list operations
-}

{-|
instance Show ModuleAst where
  -- display the ast in a readable way
  show ast = showPretty ast 0
|-} 


instance Show Stmt where
  -- display the ast in a readable way
  show ast = showPrettyStmt ast 0


instance Show Expr where
  -- display the ast in a readable way
  show ast = showPrettyExpr ast 0


  
-- | output the fully parenthesized statement
{-|
showFullyParen :: Ast
                -> String  -- ^ the fully parenthesized string representing the input Ast
showFullyParen = undefined

showFullyParenProc :: ProcAst
                -> String  -- ^ the fully parenthesized string representing the input Ast
showFullyParenProc = undefined
|-}

showFullyParenStmt :: Stmt
                -> String  -- ^ the fully parenthesized string representing the input Ast
showFullyParenStmt (Assign v b) = "(" ++ v ++ " := " ++ (showFullyParenExpr b) ++ ")"
showFullyParenStmt (If b t e) = "(if" ++ (showFullyParenExpr b) ++ " then " ++ show t ++ " else " ++ show e
showFullyParenStmt (While b d) = "(while" ++ (showFullyParenExpr b) ++ " do " ++ show d ++ ")"
showFullyParenStmt (Return b) = "(return " ++ (showFullyParenExpr b) ++ ")"

showFullyParenExpr :: Expr
                -> String  -- ^ the fully parenthesized string representing the input Ast
showFullyParenExpr (Var s) = "(" ++ s ++ ")"
showFullyParenExpr (LiteralInt i) = "(" ++ show i ++ ")"
showFullyParenExpr (Plus l r) = "(" ++ (showFullyParenExpr l) ++ "+" ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Minus l r) = "(" ++ (showFullyParenExpr l) ++ "-" ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Mult l r) = "(" ++ (showFullyParenExpr l) ++ "*" ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Div l r) = "(" ++ (showFullyParenExpr l) ++ "/" ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (LiteralBool True) = "(" ++ "true" ++ ")"
showFullyParenExpr (LiteralBool False) = "(" ++ "false" ++ ")"
showFullyParenExpr (And l r) = "(" ++ (showFullyParenExpr l) ++ " && " ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Or l r) = "(" ++ (showFullyParenExpr l) ++ " || " ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Not b) = "(" ++ " ! " ++ (showFullyParenExpr b) ++ ")"
showFullyParenExpr (Lt l r) = "(" ++ (showFullyParenExpr l) ++ " < " ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Lte l r) = "(" ++ (showFullyParenExpr l) ++ " <= " ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Gt l r) = "(" ++ (showFullyParenExpr l) ++ " > " ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Gte l r) = "(" ++ (showFullyParenExpr l) ++ " >= " ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Eq l r) = "(" ++ (showFullyParenExpr l) ++ " == " ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Ne l r) = "(" ++ (showFullyParenExpr l) ++ " != " ++ (showFullyParenExpr r) ++ ")"



-- | provide a nice show with minimal parentheses
{-|
showPretty :: Ast  -- ^ The Ast to show
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPretty = undefined

showPrettyProc :: ProcAst
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPrettyProc = undefined
|-}

showPrettyStmt :: Stmt
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPrettyStmt (Assign v b) i = parenthesize 1 i $ v ++ " := " ++ (showPrettyExpr b 1)
showPrettyStmt (If b t e) i = parenthesize 1 i $  "if " ++ (showPrettyExpr b 1) ++ " then " ++ show t ++ " else " ++ show e
showPrettyStmt (While b d) i = parenthesize 1 i $ "while " ++ (showPrettyExpr b 1) ++ " do " ++ show d
showPrettyStmt (Return b) i = parenthesize 1 i $ "return " ++ (showPrettyExpr b 1)

showPrettyExpr :: Expr
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPrettyExpr (LiteralInt i) _ = if i < 0
                                  then "(" ++ show i ++ ")"
                                  else show i
showPrettyExpr (LiteralBool True) _ = "true"
showPrettyExpr (LiteralBool False) _ = "false"
showPrettyExpr (Var s) _ = s

showPrettyExpr (Or l r) i = parenthesize 2 i $ (showPrettyExpr l 2) ++ " || " ++ (showPrettyExpr r 3)
showPrettyExpr (And l r) i = parenthesize 2 i $ (showPrettyExpr l 2) ++ " || " ++ (showPrettyExpr r 3)
showPrettyExpr (Gt l r) i = parenthesize 4 i $ (showPrettyExpr l 4) ++ " > " ++ (showPrettyExpr r 5)
showPrettyExpr (Gte l r) i = parenthesize 4 i $ (showPrettyExpr l 4) ++ " >= " ++ (showPrettyExpr r 5)
showPrettyExpr (Lte l r) i = parenthesize 4 i $ (showPrettyExpr l 4) ++ " <= " ++ (showPrettyExpr r 5)
showPrettyExpr (Lt l r) i = parenthesize 4 i $ (showPrettyExpr l 4) ++ " < " ++ (showPrettyExpr r 5)
showPrettyExpr (Ne l r) i = parenthesize 4 i $ (showPrettyExpr l 4) ++ " != " ++ (showPrettyExpr r 5)
showPrettyExpr (Eq l r) i = parenthesize 4 i $ (showPrettyExpr l 4) ++ " == " ++ (showPrettyExpr r 5)
showPrettyExpr (Minus l r) i = parenthesize 4 i $ (showPrettyExpr l 4) ++ " - " ++ (showPrettyExpr r 5)
showPrettyExpr (Plus l r) i = parenthesize 4 i $ (showPrettyExpr l 4) ++ " + " ++ (showPrettyExpr r 5)
showPrettyExpr (Mult l r) i = parenthesize 6 i $ (showPrettyExpr l 6) ++ " * " ++ (showPrettyExpr r 7)
showPrettyExpr (Div l r) i = parenthesize 6 i $ (showPrettyExpr l 6) ++ " / " ++ (showPrettyExpr r 7)
showPrettyExpr (Not l) i = parenthesize 8 i $ " ! " ++ (showPrettyExpr l 8)


