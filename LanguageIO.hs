module LanguageIO where 

import Language 
import LanguageLib
import Data.List

instance Show Expr where
    show (Var name)          = name
    show x@(Constr "S" args) = show $ numFromChurch x
    show x@(Constr "Z" args) = show $ numFromChurch x
    show (Constr name args)  = name ++ "(" ++ (intercalate ", " (map show args)) ++ ")"
    show (Lam arg expr)      = "\\" ++ arg ++ " " ++ (show expr)
    show (l :@: r)           = "(" ++ (show l) ++ " " ++ (show r) ++ ")"  
    show (GlobRef name)      = name
    show (Let (var, e1) e2)  = "let " ++ var ++ " = " ++ (show e1) ++ " in " ++ (show e2)
    show (Case expr cases)   = "case " ++ (show expr) ++ " of\n" ++ (concatMap (\(p, e) -> "    " ++ (show p) ++ " -> " ++ (show e) ++ "\n") cases)

instance Show Pat where
    show (Pat name args) = name ++ "(" ++ (intercalate ", " args) ++ ")"

instance Show Def where
    show (Def name body) = name ++ " = " ++ (show body)    

instance Show Program where
    show (Program args expr defs) = "program(" ++ (intercalate ", " args) ++ "): " ++ show expr ++ " where\n\n" ++ intercalate "\n\n" (map show defs)