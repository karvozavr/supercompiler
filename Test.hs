module Test where 

import Language
import LanguageIO
import LanguageLib
import LanguageUtil
import Interpreter
import Supercompiler
import Data.Either
import Graph

fdefTest1 = Def "Hello" (Let ("z", GlobRef "+" :@: Var "x" :@: Var "y") (Var "z"))

sum_f = Def "sum" $ Lam "xs" (Lam "a" (Case (Var "xs")
    [(Pat "Nil" [], Var "a"),  
     (Pat "Cons" ["x", "xs'"], Var "x" `plus` ((GlobRef "sum") :@: (Var "xs'") :@: (Var "a")))]))

squares_f = Def "squares" $ Lam "xs" $ Case (Var "xs") $ 
    [(nilPat, nil),
     (consPat "x" "xs'", cons (GlobRef "*" :@: Var "x" :@: Var "x") (GlobRef "squares" :@: Var "xs'"))]

upto_f = Def "upto" $ Lam "m" $ Lam "n" $ Case (GlobRef ">" :@: Var "m" :@: Var "n") $
    [(Pat "True" [], nil), 
     (Pat "False" [], cons (Var "m") (GlobRef "upto" :@: (Var "m" `plus` num 1) :@: Var "n"))]

sumOfSquares = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: num 1 :@: Var "x")) :@: num 0

sumOfSquaresProg = Program ["x"] sumOfSquares (builtins ++ [sum_f, squares_f, upto_f])

t1 = Program [] (GlobRef "+" :@: num 2  :@: num 3) builtins
t2 = Program [] (GlobRef "+" :@: num 7  :@: num 0) builtins
t3 = Program [] (GlobRef "*" :@: num 9  :@: num 4) builtins
t4 = Program [] (GlobRef ">" :@: num 9  :@: num 4) builtins
t5 = Program [] (GlobRef ">" :@: num 20 :@: num 20) builtins
t6 = Program [] (GlobRef ">" :@: num 3  :@: num 4) builtins

graph = isRight $ buildTree (Context []) (drive sumOfSquaresProg) nameSupplyInstance sumOfSquares