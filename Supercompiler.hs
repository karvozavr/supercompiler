module Supercompiler where 

import Graph
import Language
import LanguageUtil
import LanguageIO
import LanguageLib
import Interpreter
import Data.List
import Data.Either

import Debug.Trace
debug = flip trace

type Driver = NameSupply -> Expr -> (Step Expr, NameSupply)

data Context = Context { memoized :: [Expr] }  

-- Extract program from graph

data FuncDef = FuncDef { funName :: Name, body :: Expr, args :: [Name] } deriving Show

supercompile :: Program -> Program
supercompile p@(Program args e defs) = programFromGraph args graph where 
    graph = reduceDrive $ fromRight (error "got left") $ buildTree 0 (Context []) (drive (Program args e (defs ++ builtins))) nameSupplyInstance e

programFromGraph :: [Name] -> Graph Expr -> Program
programFromGraph args g = Program args e (defs ++ builtins) where
    (e, defs) = extractProgram (extractFunctions g) g

extractFunctions :: Graph Expr -> [FuncDef]
extractFunctions g = map (\(i, def) -> FuncDef ("FUNC" ++ show i) (body def) (args def)) $ zip [0..] result where 
    fs = extractFunctions' g [] []
    result = map head $ map (sortBy (\a b -> compare (length $ args b) (length $ args a))) $ groupBy (\a b -> body a == body b) fs

extractFunctions' :: Graph Expr -> [FuncDef] -> [Name] -> [FuncDef]
extractFunctions' (Node ex step) funcs args = helper funcs args' step where
    args' = case ex of 
        (Let (a, e) body) -> args ++ [a]
        _                 -> args
    
    helper :: [FuncDef] -> [Name] -> Step (Graph Expr) -> [FuncDef]
    helper funcs args (Edge n) = let fname = ("FUNC" ++ show (length funcs)) in
        if args /= [] && (not $ isLet ex) then (extractFunctions' n funcs []) ++ [FuncDef fname ex (getFreeVars [] ex)] else (extractFunctions' n funcs args)
    helper funcs args f@(Fold e ren) = let fname = ("FUNC" ++ show (length funcs)) in funcs ++ [FuncDef fname e (getFreeVars [] e)]
    helper funcs args Stop = funcs
    helper funcs args (Options edges) = let fname = ("FUNC" ++ show (length funcs)) in 
        if args /= [] 
            then (foldr (\(_, n) fs -> extractFunctions' n fs []) funcs edges) ++ [FuncDef fname ex (getFreeVars [] ex)]
            else (foldr (\(_, n) fs -> extractFunctions' n fs []) funcs edges)


extractProgram :: [FuncDef] -> Graph Expr -> (Expr, [Def])
extractProgram funcs (Node e (Edge n)) = case find (\def -> body def == e) funcs of 
    Just def -> (makeFunctionCall e def, (Def (funName def) (makeFunctionDef def e')):defs) where 
        (e', defs) = extractProgram funcs n
    Nothing  -> (ee', ddefs) where
        (ee, ddefs) = extractProgram funcs n
        ee' = case e of 
            (Let b _) -> Let b ee
            _ -> ee

extractProgram funcs (Node e (Options opts)) = case find (\def -> body def == e) funcs of 
    Just def -> (makeFunctionCall e def, (Def (funName def) (makeFunctionDef def e')):defs) where 
        (e', defs) = makeCase funcs opts
    Nothing  -> makeCase funcs opts where 

extractProgram funcs (Node e Stop) = (e, [])

extractProgram funcs (Node e (Fold parent ren)) = case find (\def -> body def == parent) funcs of 
    Just def -> (makeFunctionCall e def, [])
    Nothing  -> error "Function not found"

makeCase :: [FuncDef] -> [(Option, Graph Expr)] -> (Expr, [Def])
makeCase funcs opts = (Case (Var name) (map fst r), (concat $ map snd r)) where      
    (Option name _) = fst $ head opts   
    f ((Option name pat), node) = let (e, defs) = extractProgram funcs node in ((pat, e), defs)
    r = map f opts

makeFunctionDef :: FuncDef -> Expr -> Expr
makeFunctionDef def fbody = foldr (\a e -> Lam a e) fbody (args def)

makeFunctionCall :: Expr -> FuncDef -> Expr
makeFunctionCall e def = makeArgs (GlobRef (funName def)) (args def) where
    ren                      = maybe (error "No renaming during program extraction") id $ renaming (body def) e
    makeArgs expr []         = expr
    makeArgs expr (arg:args) = makeArgs (expr :@: (maybe (Var arg) (Var . snd) (find (\(x, _) -> x == arg) ren))) args

-- Utility functions

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

buildTree :: Int -> Context -> Driver -> NameSupply -> Expr -> Either (Expr, Expr) (Graph Expr)
buildTree depth ctx driver ns expr = algorithm where
    r = find (\expr' -> (renaming expr' expr) /= Nothing) $ memoized ctx -- if there exists memoized e' that exists the renaming Thetha e -> e'
    algorithm = case r of
        Just expr' -> (Right $ Node expr $ Fold expr' (maybe (error "No renaming for e, e'.") id $ renaming expr' expr) ) -- `debug` ("Folding \n" ++ show expr ++ "\n TO \n" ++ show expr' ++ "\n")
        Nothing -> case tryGeneralize depth ctx driver ns expr of 
            (Right node)          -> Right node
            l@(Left (e, letExpr)) -> if e == expr then furtherTransform depth ctx driver ns letExpr False else l -- если вернулся Left то обобщаем эту ноду сверху  

tryGeneralize ::  Int -> Context -> Driver -> NameSupply -> Expr -> Either (Expr, Expr) (Graph Expr)
tryGeneralize depth ctx driver ns expr = fst (algorithm, expr) where
    ext = find (\expr' -> coupling expr' expr) $ memoized ctx
    algorithm = case ext of 
        Just expr' -> result where
            (g@(Generalization gen theta1 theta2), ns') = generalize ns expr expr'
            result = if isRenaming theta2 
                then furtherTransform depth ctx driver ns' (makeLet theta1 gen) False -- generalize bottom-up \ обобщение снизу
                else Left (expr', makeLet theta2 gen) -- generalize top-down \ обобщение сверху
        Nothing -> furtherTransform depth ctx driver ns expr True

furtherTransform ::  Int -> Context -> Driver -> NameSupply -> Expr -> Bool -> Either (Expr, Expr) (Graph Expr)
furtherTransform depth ctx driver ns expr bMemoize = if depth > 200 then Right $ Node expr Stop else 
    let newCtx = Context ((memoized ctx) ++ [expr])
        oldCtx = (if isFunSubst expr then newCtx else ctx) in
    case driver ns expr of 
        (Edges edges, ns1) -> case listFromEither $ map (buildTree (depth + 1) oldCtx driver ns1) edges of
            (Right nodes) -> Right $ Node expr $ Edges nodes
            (Left x)      -> x
        (Edge e, ns1) -> case buildTree (depth + 1) oldCtx driver ns1 e of  
            (Right node) -> Right $ Node expr (Edge node)
            l@(Left _) -> l
        (Stop, ns1) -> Right $ Node expr Stop
        (Options cs, ns1) -> case listFromEither [buildTree (depth + 1) newCtx driver ns1 e | (c, e) <- cs] of
            (Right nodes) -> Right $ Node expr $ Options $ zip [c | (c, e) <- cs] nodes
            (Left x)      -> x      

drive :: Program -> Driver
drive p ns (Var _)                  = (Stop, ns)
drive p ns (Constr _ [])            = (Stop , ns)
drive p ns (Constr _ args)          = (Edges args, ns)
drive p ns (Let (_, expr1) expr2)   = (Edge expr2, ns)
drive p ns (GlobRef name)           = (Edge $ resolveRef p name, ns)
drive p ns e@((Lam arg body) :@: r) = (Edge $ body \-\ [(arg, r)] , ns)
drive p ns e@(l :@: r)              = (Edge $ intStep p e, ns)

-- Hard Part: branching
drive p ns c@(Case (Constr _ constrArgs) pats) = (Edge $ expr \-\ sub, ns) where
    (expr, argNames) = matchConstr c
    sub = zip argNames constrArgs  

drive p ns (Case v@(Var varName) pats) = (Options opts, ns1) where 
    (opts, ns1) = getOptions ns v pats

drive p ns (Case expr pats) = (inject sel, ns1) where
    (sel, ns1) = drive p ns expr
    inject (Edge t) = Edge $ Case t pats
    inject (Options options) = Options $ map f options
    f (option, e) = (option, Case e pats)
    
drive p ns e = error $ "Drive: " ++ show e

getOptions :: NameSupply -> Expr -> [(Pat, Expr)] -> ([(Option, Expr)], NameSupply)
getOptions ns v [] = ([], ns)
getOptions ns v (pat:pats) = ((opt, expr):opts, ns2) where 
    (opt, expr, ns1) = getOption ns v pat
    (opts, ns2) = getOptions ns1 v pats

getOption :: NameSupply -> Expr -> (Pat, Expr) -> (Option, Expr, NameSupply)
getOption ns (Var v) ((Pat constrName vars), body) = (Option v (Pat constrName freshVars), body \-\ sub, ns1) where
    (freshVars, ns1) = splitAt (length vars) ns
    sub = zip vars (map Var freshVars)
