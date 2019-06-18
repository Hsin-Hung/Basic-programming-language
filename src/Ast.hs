module Ast where

data Ast -- = ...


instance Show Ast where
  -- display the ast in a readable way
  show ast = showPretty ast 0

instance Eq Ast where  -- you can just derive this if you want
  _ == _ = undefined
  
  
  
-- | output the fully parenthesized statement
showFullyParen :: Ast
                -> String  -- ^ the fully parenthesized string representing the input Ast
showFullyParen = undefined

-- | provide a nice show with minimal parentheses
showPretty :: Ast  -- ^ The Ast to show
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPretty = undefined
