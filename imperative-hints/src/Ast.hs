module Ast where

type Ast = ModuleAst

-- procedure, module name and procure names with their definitions
data ModuleAst = ModuleAst String [(String, ProcAst)] -- assume the program entry point is "main" TODO: better with Map? add imports
  deriving (Eq,Show)

-- procedure, parameter names and a body
data ProcAst = ProcAst [String] Stmt  deriving (Eq,Show)

data Stmt -- = ...
{-should include: Assignment, If, While loops, Return 
and perhaps: Separators, ForEach loops
and if you're fancy Print
-}
   
data Expr -- = 
{-should include: 
Vars
procedure calls 
LitteralInt, all the int operations
LitteralBool, all the bool operations
comparisons: <, <=, ==, !=

and perhaps: Nil, Cons and list operations
-}


instance Show Stmt where
  -- display the ast in a readable way
  show ast = showPrettyStmt ast 0

instance Eq Stmt where  -- you can just derive this if you want
  _ == _ = undefined
  
instance Show Expr where
  -- display the ast in a readable way
  show ast = showPrettyExpr ast 0

instance Eq Expr where  -- you can just derive this if you want
  _ == _ = undefined
  
  
-- | output the fully parenthesized statement
showFullyParen :: Ast
                -> String  -- ^ the fully parenthesized string representing the input Ast
showFullyParen = undefined

showFullyParenProc :: ProcAst
                -> String  -- ^ the fully parenthesized string representing the input Ast
showFullyParenProc = undefined

showFullyParenStmt :: Stmt
                -> String  -- ^ the fully parenthesized string representing the input Ast
showFullyParenStmt = undefined

showFullyParenExpr :: Expr
                -> String  -- ^ the fully parenthesized string representing the input Ast
showFullyParenExpr = undefined

-- | provide a nice show with minimal parentheses
showPretty :: Ast  -- ^ The Ast to show
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPretty = undefined


showPrettyProc :: ProcAst
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPrettyProc = undefined

showPrettyStmt :: Stmt
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPrettyStmt = undefined

showPrettyExpr :: Expr
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPrettyExpr = undefined
