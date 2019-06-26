module Parser where

import Ast
import ParserMonad
import Eval


keywords = ["if","then","else", "true","false","while","do","return","call","def"]

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
parsemodule = do token $ literal "module"
                 name <- token $ varParser
                 token $ literal "{"
                 funcs <- withInfix (fmap (\x -> [x]) parseProcedureBod) [(";", (++))]
                 token $ literal "}"
                 return $ ModuleAst name funcs


{-
parse things like:
def (x,y,z){
  ...
}
-}
parseProcedureBod :: Parser (String, ProcAst)
parseProcedureBod = funcParser

funcParser = do token $ literal "def"
                name <- token $ varParser       
                token $ literal "("
                p <- (do par <- withInfix (fmap (\x -> [x]) varParser) [(",", (++))]
                         return par) <|> return []
                token $ literal ")"
                token $ literal "{"
                body <- withInfix (fmap (\x -> [x]) parseStmt) [(";", (++))]
                token $ literal "}"
                return $ (name,ProcAst p body)
                


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
parseStmt = ifParser <|> whileParser <|> returnParser <|> seqParser <|> assignParser
{-
or you can try
parseStmts :: Parser [Stmt]
parseStmts = undefined
-}
ifParser :: Parser Stmt
ifParser = do token $ literal "if"
              b <- parseExp 
              token $ literal "{"
              t <- withInfix (fmap (\x -> [x]) parseStmt) [(";", (++))]
              token $ literal "}"
              (do token $ literal "else"
                  token $ literal "{"
                  f <- withInfix (fmap (\x -> [x]) parseStmt) [(";", (++))]  
                  token $ literal "}"
                  let res = If b t f 
                  return res) <|> return (If b t [])


assignParser :: Parser Stmt
assignParser = do s <- token $ varParser
                  token (literal ":=")
                  exp <- parseExp
                  let res = s `Assign` exp
                  return res
 

whileParser :: Parser Stmt
whileParser = do token $ literal "while"
                 e <- parseExp
                 token $ literal "{"
                 d <- withInfix (fmap (\x -> [x]) parseStmt) [(";", (++))]
                 token $ literal "}"
                 return $ While e d

returnParser :: Parser Stmt
returnParser = do token $ literal "return"
                  r <- parseExp
                  return $ Return r

seqParser :: Parser Stmt
seqParser = do token $ literal "{"
               s <- withInfix (fmap (\x -> [x]) parseStmt) [(";", (++))]
               token $ literal "}"
               return $ Seq s
{-
parse things like:
x+7%23 /4 == 3 || x != y && f(1,x,3+80) == true
-}
parseExp :: Parser Expr
parseExp = orExpr

vars :: Parser Expr
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

ints :: Parser Expr
ints = do i <- token $ intParser
          return $ LiteralInt i

parens :: Parser Expr
parens = do token $ literal "("
            ast <- parseExp
            token $ literal ")"
            return ast


bools :: Parser Expr
bools = do b <- token $ literal "true" <|> literal "false"
           case b of 
            "true" -> return $ LiteralBool True
            _      -> return $ LiteralBool False


orExpr :: Parser Expr
orExpr = withInfix andExpr [("||",Or)] 

andExpr :: Parser Expr
andExpr= withInfix ltLteExpr [("&&",And)] 


ltLteExpr :: Parser Expr
ltLteExpr = withInfix gtGteExpr [("<=",Lte),("<",Lt)]

gtGteExpr :: Parser Expr
gtGteExpr = withInfix eqNeExpr [(">=",Gte),(">",Gt)]

eqNeExpr :: Parser Expr
eqNeExpr = withInfix addSubExpr [("==",Eq),("/=",Ne)]

addSubExpr :: Parser Expr
addSubExpr = withInfix multDivExpr [("+",Plus),("-",Minus)] 

multDivExpr :: Parser Expr
multDivExpr= withInfix modExpr [("*",Mult),("/",Div)] 

modExpr :: Parser Expr
modExpr = withInfix expExpr [("%",Mod)]

expExpr :: Parser Expr
expExpr = withInfix notExpr [("^",Exp)]

notExpr :: Parser Expr
notExpr = (do token (literal "!")
              b <- notExpr <|> atoms
              let res = Not b
              return res) <|> atoms

procCall :: Parser Expr
procCall = do f <- token $ varParser
              (do token $ literal "("
                  args <- withInfix (fmap (\x -> [x]) parseExp) [(",", (++))]
                  token $ literal  ")"
                  return (Call f args)) <|> (return $ Var f )

atoms:: Parser Expr
atoms = ints <|> bools <|> parens <|> procCall <|> vars 