module Test where 

import Language
import LanguageIO
import LanguageLib
import LanguageUtil
import Interpreter
import Supercompiler
import Data.Either
import Graph
import Debug.Trace
import Data.List


fdefTest1 = Def "Hello" (Let ("z", GlobRef "+" :@: Var "x" :@: Var "y") (Var "z"))

-- Self apply

fff = Def "FUNC0" (Lam "A" $ Lam "N" $ Lam "B" (Case (GlobRef ">" :@: Var "A" :@: Var "N") [
  (Pat "True" [], Var "B"), (Pat "False" [], GlobRef "FUNC0" :@: (GlobRef "+" :@: Var "A" :@: num 1) :@: Var "N" :@: (GlobRef "+" :@: Var "B" :@: (GlobRef "*" :@: Var "A" :@: Var "A"))) ])) 

fffexpr = GlobRef "FUNC0" :@: num 1 :@: Var "N" :@: num 0

fffprog = Program ["N"] fffexpr ([fff] ++ builtins)

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

sumOfSquaresProg = Program ["N"] sumOfSquares (builtins ++ [sum_f, squares_f, upto_f])

t1 = Program [] (GlobRef "+" :@: num 2  :@: num 3) builtins
t2 = Program [] (GlobRef "+" :@: num 7  :@: num 0) builtins
t3 = Program [] (GlobRef "*" :@: num 9  :@: num 4) builtins
t4 = Program [] (GlobRef ">" :@: num 9  :@: num 4) builtins
t5 = Program [] (GlobRef ">" :@: num 20 :@: num 20) builtins
t6 = Program [] (GlobRef ">" :@: num 3  :@: num 4) builtins

graph = isRight $ buildTree 0 (Context []) (drive sumOfSquaresProg) nameSupplyInstance sumOfSquares


aTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: num 1 :@: Var "n")) :@: num 0
bTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: (GlobRef "+" :@: num 1 :@: num 1) :@: Var "n"))
  :@: (GlobRef "+" :@: (num 0) :@: (GlobRef "*" :@: num 1 :@: num 1))
cTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: (GlobRef "+" :@: Var "x1" :@: num 1) :@: Var "n"))
  :@: (GlobRef "+" :@: Var "x2" :@: (GlobRef "*" :@: Var "x1" :@: Var "x1"))
dTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: Var "x1" :@: Var "n")) :@: Var "x2"
eTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: Var "ABBA" :@: Var "N")) :@: Var "CDDC"
sosTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: num 1 :@: Var "x")) :@: num 0

testFunctionExtraction 
  = extractFunctions (reduceDrive $ fromRight (error "got left") $ buildTree 0 (Context []) (drive sumOfSquaresProg) nameSupplyInstance sumOfSquares)

foldTest = renderDot $ reduceDrive $ fromRight (error "got left") $ buildTree 0 (Context []) (drive sumOfSquaresProg) nameSupplyInstance dTest
sumSquaresTest = reduceDrive $ fromRight (error "got left") $ buildTree 0 (Context []) (drive sumOfSquaresProg) nameSupplyInstance sumOfSquares
resultingProgram = supercompile sumOfSquaresProg


----------------
--- RUN THIS ---
----------------
renderConfigGraph filename = writeFile filename (renderDot sumSquaresTest)
----------------
