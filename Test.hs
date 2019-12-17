module Test where 

import Language
import LanguageIO
import LanguageLib

fdefTest1 = Def "Hello" (Let ("z", FunCall "+" [Var "x", Var "y"]) (Var "z"))

sum_f = Def "sum" $ Lam "xs" (Lam "a" (Case (Var "xs")
    [(Pat "Nil" [], Var "a"),  
     (Pat "Cons" ["x", "xs"], FunCall "plus" [Var "x", (Var "sum") :@: (Var "xs") :@: (Var "a")])]))

squares_f = Def "squares" $ Lam "xs" $ Case (Var "xs") $ 
    [(nilPat, nil),
     (consPat "x" "xs", cons (FunCall "*" [Var "x", Var "x"]) (Var "squares" :@: Var "xs"))]

upto_f = Def "upto" $ Lam "m" $ Lam "n" $ Case (FunCall ">" [Var "m", Var "n"]) $
    [(Pat "True" [], nil), 
     (Pat "False" [], cons (Var "m") (Var "upto" :@: (Var "m" `plus` num 1) :@: Var "n"))]

sumOfSquares = Var "sum" :@: (Var "squares" :@: (Var "upto" :@: num 1 :@: Var "n"))

sumOfSquaresProg = Program sumOfSquares [sum_f, squares_f, upto_f]