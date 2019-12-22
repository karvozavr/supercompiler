module Supercompiler where 

import Graph
import Language
import LanguageUtil
import Interpreter
import Data.List
import Data.Either

import Debug.Trace
debug = flip trace

type Driver = NameSupply -> Expr -> Step Expr

data Context = Context { memoized :: [Expr] }  

isRenaming :: Subst -> Bool
isRenaming sub = all f sub where
    f (v, Var _) = True
    f _          = False

makeLet :: Subst -> Expr -> Expr
makeLet sub expr = foldr f expr sub where 
    f (v, e) letExpr = Let (v, e) letExpr 

listFromEither :: [Either a b] -> Either (Either a b) [b]
listFromEither l = if all isRight l
    then Right $ map (fromRight (error "Is Left during listFromEither")) l
    else Left $ maybe (error "From maybe in listFromEither") id $ find isLeft l

-- Building configuration graph

buildTree :: Context -> Driver -> NameSupply -> Expr -> Either (Expr, Expr) (Graph Expr)
buildTree ctx driver ns expr = algorithm where
    r = find (\expr' -> (renaming expr' expr) /= Nothing) $ memoized ctx `debug` ("debug: " ++ show expr ++ "\n") -- if there exists memoized e' that exists the renaming Thetha e -> e'
    algorithm = case r of
        Just expr' -> Right $ Node expr $ Fold expr' (maybe (error "No renaming for e, e'.") id $ renaming expr' expr)  
        Nothing -> case tryGeneralize ctx driver ns expr of 
            (Right node)          -> Right node
            l@(Left (e, letExpr)) -> if e == expr then furtherTransform ctx driver ns letExpr else l -- если вернулся Left то обобщаем эту ноду сверху  

tryGeneralize :: Context -> Driver -> NameSupply -> Expr -> Either (Expr, Expr) (Graph Expr)
tryGeneralize ctx driver ns expr = algorithm where 
    ext = find (\expr' -> coupling expr expr') $ memoized ctx -- TODO homeomorphic embedding
    algorithm = case ext of 
        Just expr' -> result where
            (g@(Generalization gen theta1 theta2), ns') = generalize ns expr expr'
            result = if isRenaming theta2 
                then furtherTransform ctx driver ns' (makeLet theta1 gen) `debug` ("сверху: " ++ show gen ++ "\n")  -- generalize bottom-up \ обобщение снизу
                else Left (expr', makeLet theta2 gen) -- generalize top-down \ обобщение сверху
        Nothing -> furtherTransform (Context ((memoized ctx) ++ [expr])) driver ns expr

furtherTransform :: Context -> Driver -> NameSupply -> Expr -> Either (Expr, Expr) (Graph Expr)
furtherTransform ctx driver ns expr = case driver ns expr of 
    Edges edges -> case listFromEither $ map (buildTree ctx driver ns) edges of
        (Right nodes) -> Right $ Node expr $ Edges nodes
        (Left x)      -> x
    Edge e -> case buildTree ctx driver ns e of  
        (Right node) -> Right $ Node expr (Edge node)
        l@(Left _) -> l
    Stop -> Right $ Node expr Stop
    Options cs -> case listFromEither [buildTree ctx driver ns e | (c, e) <- cs] of
        (Right nodes) -> Right $ Node expr $ Options $ zip [c | (c, e) <- cs] nodes
        (Left x)      -> x  

drive :: Program -> Driver
drive p ns (Var _)                  = Stop
drive p ns (Constr _ [])            = Stop 
drive p ns (Constr _ args)          = Edges args
drive p ns (Let (_, expr1) expr2)   = Edges [expr1, expr2]
drive p ns (GlobRef name)           = Edge $ resolveRef p name
drive p ns e@((Lam arg body) :@: r) = Edge $ intStep p e
drive p ns e@(l :@: r)              = Edge $ intStep p e

-- Hard Part: branching
drive p ns c@(Case (Constr _ constrArgs) pats) = Edge $ expr \-\ sub where
    (expr, argNames) = matchConstr c
    sub = zip argNames constrArgs  

drive p ns (Case v@(Var varName) pats) = Options $ map (getOption ns v) pats

drive p ns (Case expr pats) = inject $ drive p ns expr where
    inject (Edge t) = Edge $ Case t pats
    inject (Options options) = Options $ map f options
    f (option, e) = (option, Case e pats)
    
drive p ns e = error $ "Drive: " ++ show e


getOption :: NameSupply -> Expr -> (Pat, Expr) -> (Option, Expr)
getOption ns (Var v) ((Pat constrName vars), body) = (Option v (Pat constrName freshVars), body \-\ sub) where
    freshVars = take (length vars) ns
    sub = zip vars (map Var freshVars)