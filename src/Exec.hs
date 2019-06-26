module Exec where

import Data.Set (Set)
import qualified Data.Set as Set

import Ast
import Eval
import Parser
import ParserMonad



data LangOut = 
   ParseError -- ^ retuned when the string could not be parsed
 | RuntimeError String
-- ^ retuned when there is a runtime error
-- first String is the error message
 | Ok Val
-- ^ retuned when the program runs successfully and return a value
 deriving (Show, Eq)



-- | execute the program as a string and get the result
exec :: String -> LangOut
exec s = case (parse parser) s of
           Just (ast,"") -> case run ast of
                              Right v -> Ok v
                              Left e -> RuntimeError e
           _ -> ParseError

runFile path = 
  do program <- readFile path
     return $ exec program

parseFile path = 
  do program <- readFile path
     return $ parse parsemodule program

test1 = exec "module main{\
                \def p1(){\
                  \counter:=0;\
                  \out:=0;\
                  \while (counter<1000) {\
                    \if ((counter%3==0) || (counter%5==0)){\
                      \out:=out+counter};\
                    \counter:=counter+1};\
                  \return out}}"

test2 = exec "module main{\
               \def p2(){\
                 \prev:=1;\
                 \temp:=0;\
                 \val:=2;\
                 \total:=0;\
                 \while (val<=4000000){\
                   \if(val%2==0){\
                     \total:=total+val};\
                     \temp:=prev;\
                     \prev:=val;\
                     \val:=val+temp};\
                 \return total}}"

test3 = exec "module main{\
               \def p3(){\
                 \num:=600851475143;\
                 \while(true){\
                   \sf:=p3help(num);\
                   \if(sf<num){\
                     \num:=num/sf}\
                   \else{return num}}};\
                \def p3help(n){\
                  \x:=2;\
                  \while(x<n){\
                    \if(n%x==0){\
                       \return x};\
                    \x:=x+1};\
                  \return n}}"

test5 = exec "module main{\
                \def p5(){\
                  \i:=1;\
                  \ret:=1;\
                  \while(i<=20){\
                    \ret:=ret*(i/p5help(i,ret));\
                    \i:=i+1};\
                  \return ret};\
                \def p5help(a,b){\
                  \if(b==0){\
                    \return a}\
                  \else{\
                    \return p5help(b,a%b)}\
                 \}}"
  
test6 = exec "module main{\
                \def p6(){\
                  \i:=1;\
                  \r1:=0;\
                  \r2:=(((1+100)*100)/2)^2;\
                  \while(i<=100){\
                     \r1:=r1+i^2;\
                     \i:=i+1};\
                  \return r2-r1}}"

test9 = exec "module main{\
                \def p9(){\
                  \a:=1;\
                  \while (a<= (1000/3)){\
                     \b:=a+1;\
                     \while (b <= (1000/2)){\
                        \c:=1000-a-b;\
                        \if((a^2)+(b^2)==(c^2)){\
                        \return a*b*c};\
                        \b:=b+1};\
                     \a:=a+1}}}"