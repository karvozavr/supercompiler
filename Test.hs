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
     (Pat "Cons" ["x", "xs'"], ((GlobRef "sum") :@: (Var "xs'") :@: (Var "a" `plus` Var "x")))]))

squares_f = Def "squares" $ Lam "xs" $ Case (Var "xs") $ 
    [(nilPat, nil),
     (consPat "x" "xs'", cons (GlobRef "*" :@: Var "x" :@: Var "x") (GlobRef "squares" :@: Var "xs'"))]

upto_f = Def "upto" $ Lam "m" $ Lam "n" $ Case (GlobRef ">" :@: Var "m" :@: Var "n") $
    [(Pat "True" [], nil), 
     (Pat "False" [], cons (Var "m") (GlobRef "upto" :@: (Var "m" `plus` num 1) :@: Var "n"))]

sumOfSquares = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: num 1 :@: Var "N")) :@: num 0

sumOfSquaresProg = Program ["x"] sumOfSquares (builtins ++ [sum_f, squares_f, upto_f])

t1 = Program [] (GlobRef "+" :@: num 2  :@: num 3) builtins
t2 = Program [] (GlobRef "+" :@: num 7  :@: num 0) builtins
t3 = Program [] (GlobRef "*" :@: num 9  :@: num 4) builtins
t4 = Program [] (GlobRef ">" :@: num 9  :@: num 4) builtins
t5 = Program [] (GlobRef ">" :@: num 20 :@: num 20) builtins
t6 = Program [] (GlobRef ">" :@: num 3  :@: num 4) builtins

graph = isRight $ buildTree 0 (Context []) (drive sumOfSquaresProg) nameSupplyInstance sumOfSquares

-- firstFold (Node a ()) = 

-- Tests

aTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: num 1 :@: Var "n")) :@: num 0

bTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: (GlobRef "+" :@: num 1 :@: num 1) :@: Var "n"))
  :@: (GlobRef "+" :@: (num 0) :@: (GlobRef "*" :@: num 1 :@: num 1))

cTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: (GlobRef "+" :@: Var "x1" :@: num 1) :@: Var "n"))
  :@: (GlobRef "+" :@: Var "x2" :@: (GlobRef "*" :@: Var "x1" :@: Var "x1"))

dTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: Var "x1" :@: Var "n")) :@: Var "x2"

eTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: Var "ABBA" :@: Var "N")) :@: Var "CDDC"

sosTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: num 1 :@: Var "x")) :@: num 0

foldTest = renderDot $ fromRight (error "got left") $ buildTree 0 (Context []) (drive sumOfSquaresProg) nameSupplyInstance dTest

ge1 = Lam "x" $ Lam "y" $ Var "y" :@: Var "x"
ge2 = Lam "x" $ Lam "y" $ Var "x" :@: Var "y"