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

buildTree :: Int -> Context -> Driver -> NameSupply -> Expr -> Either (Expr, Expr) (Graph Expr)
buildTree depth ctx driver ns expr = algorithm where -- `debug` ("Debug: " ++ show expr) where
    r = find (\expr' -> (renaming expr' expr) /= Nothing) $ memoized ctx -- `debug` ("Debug: " ++ show expr) -- if there exists memoized e' that exists the renaming Thetha e -> e'
    algorithm = case r of
        Just expr' -> Right $ Node expr $ Fold expr' (maybe (error "No renaming for e, e'.") id $ renaming expr' expr) `debug` ("Folding \n" ++ show expr ++ "\n TO \n" ++ show expr' ++ "\n")
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
                then furtherTransform depth ctx driver ns' (makeLet theta1 gen) False -- `debug` ("Generalize \n" ++ show expr ++ "\n TO \n" ++ show expr' ++ "\n") -- generalize bottom-up \ обобщение снизу
                else Left (expr', makeLet theta2 gen) `debug` ("Сверху") -- generalize top-down \ обобщение сверху
        Nothing -> furtherTransform depth ctx driver ns expr True

furtherTransform ::  Int -> Context -> Driver -> NameSupply -> Expr -> Bool -> Either (Expr, Expr) (Graph Expr)
furtherTransform depth ctx driver ns expr bMemoize = if depth > 50 then Right $ Node expr Stop else 
    let newCtx = Context ((memoized ctx) ++ [expr])
        oldCtx = (if isFunSubst expr then newCtx else ctx) in
    case driver ns expr of 
        Edges edges -> case listFromEither $ map (buildTree (depth + 1) oldCtx driver ns) edges of
            (Right nodes) -> Right $ Node expr $ Edges nodes
            (Left x)      -> x
        Edge e -> case buildTree (depth + 1) oldCtx driver ns e of  
            (Right node) -> Right $ Node expr (Edge node)
            l@(Left _) -> l
        Stop -> Right $ Node expr Stop
        Options cs -> case listFromEither [buildTree (depth + 1) newCtx driver ns e | (c, e) <- cs] of
            (Right nodes) -> Right $ Node expr $ Options $ zip [c | (c, e) <- cs] nodes
            (Left x)      -> x      

drive :: Program -> Driver
drive p ns (Var _)                  = Stop
drive p ns (Constr _ [])            = Stop 
drive p ns (Constr _ args)          = Edges args
drive p ns (Let (_, expr1) expr2)   = Edge expr2
drive p ns (GlobRef name)           = Edge $ resolveRef p name
drive p ns e@((Lam arg body) :@: r) = Edge $ body \-\ [(arg, r)] 
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
    sub = zip vars (map Var vars) -- TODOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO CHECK THIS


-- Check for function substitution 
isFunSubst :: Expr -> Bool
isFunSubst (Var _)                  = False
isFunSubst (Constr _ [])            = False 
isFunSubst (Constr _ args)          = False
isFunSubst (Let (_, expr1) expr2)   = False
isFunSubst (GlobRef name)           = True
isFunSubst e@((Lam arg body) :@: r) = False 
isFunSubst e@(l :@: r)              = isFunSubst l
isFunSubst c@(Case (Constr _ constrArgs) pats) = False 
isFunSubst (Case v@(Var varName) pats) = False 
isFunSubst (Case expr pats) = isFunSubst expr
    --------------------------- TESTS __________________________


-- fdefTest1 = Def "Hello" (Let ("z", GlobRef "+" :@: Var "x" :@: Var "y") (Var "z"))

-- sum_f = Def "sum" $ Lam "xs" (Lam "a" (Case (Var "xs")
--     [(Pat "Nil" [], Var "a"),  
--         (Pat "Cons" ["x", "xs'"], ((GlobRef "sum") :@: (Var "xs'") :@: (Var "a" `plus` Var "x")))]))

-- squares_f = Def "squares" $ Lam "xs" $ Case (Var "xs") $ 
--     [(nilPat, nil),
--         (consPat "x" "xs'", cons (GlobRef "*" :@: Var "x" :@: Var "x") (GlobRef "squares" :@: Var "xs'"))]

-- upto_f = Def "upto" $ Lam "m" $ Lam "n" $ Case (GlobRef ">" :@: Var "m" :@: Var "n") $
--     [(Pat "True" [], nil), 
--         (Pat "False" [], cons (Var "m") (GlobRef "upto" :@: (Var "m" `plus` num 1) :@: Var "n"))]

-- sumOfSquares = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: num 1 :@: Var "N")) :@: num 0

-- sumOfSquaresProg = Program ["x"] sumOfSquares (builtins ++ [sum_f, squares_f, upto_f])

-- t1 = Program [] (GlobRef "+" :@: num 2  :@: num 3) builtins
-- t2 = Program [] (GlobRef "+" :@: num 7  :@: num 0) builtins
-- t3 = Program [] (GlobRef "*" :@: num 9  :@: num 4) builtins
-- t4 = Program [] (GlobRef ">" :@: num 9  :@: num 4) builtins
-- t5 = Program [] (GlobRef ">" :@: num 20 :@: num 20) builtins
-- t6 = Program [] (GlobRef ">" :@: num 3  :@: num 4) builtins

-- graph = isRight $ buildTree (Context []) (drive sumOfSquaresProg) nameSupplyInstance sumOfSquares

-- -- firstFold (Node a ()) = 

-- -- Tests

-- aTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: num 1 :@: Var "n")) :@: num 0

-- bTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: (GlobRef "+" :@: num 1 :@: num 1) :@: Var "n"))
--     :@: (GlobRef "+" :@: (num 0) :@: (GlobRef "*" :@: num 1 :@: num 1))

-- cTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: (GlobRef "+" :@: Var "x1" :@: num 1) :@: Var "n"))
--     :@: (GlobRef "+" :@: Var "x2" :@: (GlobRef "*" :@: Var "x1" :@: Var "x1"))

-- dTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: Var "x1" :@: Var "n")) :@: Var "x2"

-- eTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: Var "ABBA" :@: Var "N")) :@: Var "CDDC"

-- sosTest = GlobRef "sum" :@: (GlobRef "squares" :@: (GlobRef "upto" :@: num 1 :@: Var "x")) :@: num 0

-- foldTest = buildTree (Context []) (drive sumOfSquaresProg) nameSupplyInstance dTest