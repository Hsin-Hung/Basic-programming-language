module Parser where

import Ast
import ParserMonad


-- | parser for the language
parser :: Parser Ast
parser = parsemodule


{-
parse things like:

module name {
  def ...
  def ...
  def ...
}
-}
parsemodule :: Parser ModuleAst
parsemodule = undefined


{-
parse things like:

def (x,y,z){
  ...
}
-}
parseProcedureBod :: Parser (String, ProcAst)
parseProcedureBod = undefined


{-
parse things like:

x := ... ; retrun ...

or like:
while (...){
  ...
}

or like:
if(...){
...
}else{
...
}
-}


parseStmt :: Parser Stmt
parseStmt = undefined
{-
or you can try
parseStmts :: Parser [Stmt]
parseStmts = undefined
-}





{-
parse things like:

x+7%23 /4 == 3 || x != y && f(1,x,3+80) == true
-}
parseExp :: Parser Expr
parseExp = undefined
