module Supercompiler where 

import Graph
import Language
import LanguageUtil
import Interpreter

type Context = { memoized :: [Expr] }  

tryFold :: Graph Expr -> Context -> Expr -> Step (Graph Expr)
tryFold = undefined

buildTreeStep :: Context -> Program -> Expr -> Step Expr
buildTreeStep ctx p expression = makeStep expression where
    makeStep :: Expr -> Step (Graph Expr)
    makeStep (Var _)                  = Stop
    makeStep (Constr _ [])            = Stop 
    makeStep (Constr _ args)          = Edges args
    makeStep (FunCall _ args)         = Edges args
    makeStep (GlobRef name)           = Edge $ resolveRef p name
    makeStep e@((Lam arg body) :@: r) = Edge $ intStep p e
    makeStep e@(l :@: r)              = Edge $ intStep p e
    makeStep (Let (_, expr1) expr2)   = Edges [expr1, expr2]
    makeStep (Case e pats)            = Options $ map (\(pat, exp) -> (Option e pat, exp)) pats
