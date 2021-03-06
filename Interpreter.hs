module Interpreter where

import Language 
import LanguageUtil
import LanguageLib
import LanguageIO
import Data.List

interpret :: Program -> [Expr] -> Expr
interpret prog@(Program argnames expr defs) args = until isValue (intStep prog) (expr \-\ (zip argnames args))

intStep :: Program -> Expr -> Expr
intStep p (Constr name args) =
    Constr name (values ++ ((intStep p x) : xs)) where
      (values, x:xs) = span isValue args 

intStep p ((Lam arg body) :@: r) = body \-\ [(arg, r)] 

intStep p (Let (name, expr) body) = body \-\ [(name, expr)]

intStep p (l :@: r) = (intStep p l) :@: r

intStep p (Case e pats) 
    | not $ isValue e = Case (intStep p e) pats

intStep p caseExpr@(Case c@(Constr name args) pats) = 
    expr \-\ (zip argsP args) where
        res         = find (\((Pat nameP _), _) -> name == nameP) pats
        ((Pat _ argsP), expr) = maybe (error ("No pattern found for " ++ name ++ "\n" ++ show caseExpr)) id res

intStep p (GlobRef name) = resolveRef p name

intStep p e = error (show e)