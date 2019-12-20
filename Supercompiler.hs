module Supercompiler where 

import Graph
import Language
import LanguageUtil
import Interpreter
import Data.List

type NameSupply = [Name]
type Driver = NameSupply -> Expr -> Step Expr

data Context = Context { memoized :: [Expr] }  

buildTree :: Context -> Driver -> NameSupply -> Expr -> Graph Expr
buildTree ctx driver (name:ns) expr = Node expr Stop where
    r = find (\memE -> (renaming memE expr) /= Nothing) $ memoized ctx
    algorithm = case r of 
        Just ren -> undefined
        Nothing  -> undefined

furtherTransform ctx driver ns expr = case driver ns expr of 
    Edges edges -> Node expr $ Edges $ map (buildTree ctx driver ns) edges 

getDriver :: Program -> Driver
getDriver p = drive where
    drive :: Driver
    drive ns (Var _)                  = Stop
    drive ns (Constr _ [])            = Stop 
    drive ns (Constr _ args)          = Edges args
    drive ns (Let (_, expr1) expr2)   = Edges [expr1, expr2]
    drive ns (GlobRef name)           = Edge $ resolveRef p name
    drive ns e@((Lam arg body) :@: r) = Edge $ intStep p e
    drive ns e@(l :@: r)              = Edge $ intStep p e

    -- Hard Part: branching
    drive ns c@(Case (Constr _ constrArgs) pats) = Edge $ expr \-\ sub where
        (expr, argNames) = matchConstr c
        sub = zip argNames constrArgs  
    
    drive ns (Case v@(Var varName) pats) = Options $ map (getOption ns v) pats

    drive ns (Case expr pats) = inject $ drive ns expr where
        inject (Edge t) = Edge $ Case t pats
        inject (Options options) = Options $ map f options
        f (option, e) = (option, Case e pats)


getOption :: NameSupply -> Expr -> (Pat, Expr) -> (Option, Expr)
getOption ns (Var v) ((Pat constrName vars), body) = (Option v (Pat constrName freshVars), body \-\ sub) where
    freshVars = take (length vars) ns
    sub = zip vars (map Var freshVars)