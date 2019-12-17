module Interpreter where

import Language 
import LanguageUtil
import LanguageLib
import LanguageIO
import Data.List

interpret :: Program -> [Expr] -> Expr
interpret prog@(Program argnames expr defs) args = until isValue intStep (expr \-\ (zip argnames args) \-\ substFromDefs defs)

int prog@(Program argnames expr defs) args = (zip argnames args) ++ (substFromDefs defs)

substFromDefs :: [Def] -> Subst 
substFromDefs = map (\(Def name body) -> (name, body)) 

intStep :: Expr -> Expr
intStep (Constr name args) =
    Constr name (values ++ ((intStep x) : xs)) where
      (values, x:xs) = span isValue args 

intStep ((Lam arg body) :@: r) = body \-\ [(arg, r)] 

intStep (Let (name, expr) body) = body \-\ [(name, expr)]

intStep (l :@: r) = (intStep l) :@: r

intStep (Case e pats) 
    | isValue e = Case (intStep e) pats

intStep (Case c@(Constr name args) pats) = 
    expr \-\ (zip argsP args) where
        res         = find (\((Pat nameP _), _) -> name == nameP) pats
        ((Pat _ argsP), expr) = maybe (error ("No pattern found for " ++ name)) id res

intStep (FunCall name args) = case span isValue args of 
    (values, [])   -> f args where 
        f = maybe (error ("No such function " ++ name)) id (lookup name builtins)
    (values, x:xs) -> FunCall name (values ++ ((intStep x) : xs))

intStep x = error $ "\n\n" ++ show x 