module LanguageIO where 

import Language
import Data.List

instance Show Expr where
    show (Var name)       = name
    show (Constr name args) = name ++ "(" ++ (intercalate ", " (map show args)) ++ ")"
    show (FunCall name args)   = name ++ "(" ++ (intercalate ", " (map show args)) ++ ")"
    show (Let (var, e1) e2) = "let " ++ var ++ " = " ++ (show e1) ++ " in " ++ (show e2)

instance Show FunDef where
    show (FunDef name args body) = name ++ "(" ++ intercalate ", " args ++ ") = " ++ (show body)