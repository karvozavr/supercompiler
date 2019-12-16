module Test where 

import Language
import LanguageIO

fdefTest1 = FunDef "Hello" ["x", "y"] (Let ("z", FunCall "plus" [Var "x", Var "y"]) (Var "z"))
