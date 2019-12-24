module LanguageUtil where

import Language
import LanguageIO
import LanguageLib
import Data.List
import Data.Maybe
import Control.Monad.State

type NameSupply = [Name]

nameSupplyInstance = ["_VAR_" ++ (show x) | x <- [0..]]

getFreeVars :: Expr -> [Name] 
getFreeVars (Var x) = [] 

-- Substitution operator
infixl 5 \-\

(\-\) :: Expr -> Subst -> Expr
(Var x)             \-\ sub = maybe (Var x) id (lookup x sub)
(Constr name args)  \-\ sub = Constr name (map (\-\ (filter (\(n, _) -> n /= name) sub)) args)
(Lam name body)     \-\ sub = Lam name (body \-\ (filter (\(x, _) -> x /= name) sub))
(Let (x, e1) e2)    \-\ sub = error "Let does not support substitution" -- Let (x, (e1 \-\ sub)) (e2 \-\ sub)
(Case e cases)      \-\ sub = Case (e \-\ sub) (map f cases) where
  f (p@(Pat _ args), ei) = (p, ei \-\ (filter (\(n, _) -> not $ n `elem` args) sub))
(l :@: r)           \-\ sub = (l \-\ sub) :@: (r \-\ sub)
ref@(GlobRef _)     \-\ sub = ref

isValue :: Expr -> Bool
isValue (Constr _ args) = and $ map isValue args
isValue _ = False

resolveRef :: Program -> Name -> Expr 
resolveRef (Program _ _ defs) name = val where
    (Def _ val) = maybe (error ("Unresolved Reference " ++ name)) id (find (\(Def name' val) -> name' == name) defs)

matchConstr :: Expr -> (Expr, [Name])
matchConstr (Case (Constr name vars) pats) = head [(e, pVars) | ((Pat pName pVars), e) <- pats, name == pName]

-- TODO
renaming :: Expr -> Expr -> Maybe Renaming
renaming e1 e2 = f $ partition isNothing $ renaming' (e1, e2) where
  f (x:_, _) = Nothing
  f (_, ps) = g gs1 gs2
    where
      gs1 = groupBy (\(a, b) (c, d) -> a == c) $ sortBy h $ nub $ catMaybes ps
      gs2 = groupBy (\(a, b) (c, d) -> b == d) $ sortBy h $ nub $ catMaybes ps
      h (a, b) (c, d) = compare a c
  g xs ys = if all ((== 1) . length) xs && all ((== 1) . length) ys
    then Just (concat xs) else Nothing

renaming' :: (Expr, Expr) -> [Maybe (Name, Name)]
renaming' ((Var x), (Var y)) = [Just (x, y)]
renaming' ((Constr n1 args1), (Constr n2 args2)) | n1 == n2 = concat $ map renaming' $ zip args1 args2
renaming' ((GlobRef n1), (GlobRef n2)) = [Just (n1, n2)]
renaming' (Let (v, e1) e2, Let (v', e1') e2') = renaming' (e1, e1') ++ renaming' (e2, e2' \-\ [(v, Var v')])
renaming' (a1 :@: b1, a2 :@: b2) = renaming' (a1, a2) ++ renaming' (b1, b2) 
renaming' (Lam x e1, Lam y e2) = renaming' (e1, e2 \-\ [(x, Var y)])
renaming' (Case x pats1, Case y pats2) | patsEqual pats1 pats2 = 
    renaming' (x, y) ++ (concat $ map renaming' $ zip (map snd pats1) (map snd pats2))
renaming' _  = [Nothing]

patsEqual :: [(Pat, Expr)] -> [(Pat, Expr)] -> Bool
patsEqual p1 p2 = all (\(Pat name1 _, Pat name2 _) -> name1 == name2) $ zip (map fst p1) (map fst p2)

-- Most Specific Generalization Algorithm

data Generalization = Generalization { expression :: Expr, subst1 :: Subst, subst2 :: Subst } deriving Show

generalizeList :: NameSupply -> [(Expr, Expr)] -> ([Generalization], NameSupply)
generalizeList names pairs = foldr f ([], names) pairs where
  f (e, e') (acc, ns) = ((gen:acc), ns') where
    (gen, ns') = generalize ns e e'   

trivialGen :: NameSupply -> Expr -> Expr -> (Generalization, NameSupply)
trivialGen (n:ns) e1 e2 = (Generalization (Var n) [(n, e1)] [(n, e2)], ns) 

generalize :: NameSupply -> Expr -> Expr -> (Generalization, NameSupply)
generalize ns (Var a) (Var b) = (Generalization (Var a) [] [], ns) 

generalize ns g1@(GlobRef n1) g2@(GlobRef n2) | n1 == n2 = (Generalization g1 [] [], ns)

generalize ns (Constr name args) (Constr name' args') | name == name' 
  = (Generalization expr theta' theta'', ns') where
      (gens, ns') = generalizeList ns (zip args args')
      expr        = Constr name $ map expression gens
      theta'      = concat $ map subst1 gens
      theta''     = concat $ map subst2 gens

generalize ns (e1' :@: e2') (e1'' :@: e2'') 
  = (Generalization (e1 :@: e2) (theta1' ++ theta2') (theta1'' ++ theta2''), ns2) where 
    (Generalization e1 theta1' theta1'', ns1) = generalize ns e1' e1''
    (Generalization e2 theta2' theta2'', ns2) = generalize ns1 e2' e2''

generalize ns (Lam v1 body1) (Lam v2 body2) 
  = (Generalization (Lam freshVarName (expression gen)) (subst1 gen) (subst2 gen), ns2) where -- TODO check validity of generalization
    (freshVarName:ns1) = ns
    freshVar           = Var freshVarName
    freshBody1         = body1 \-\ [(v1, freshVar)]
    freshBody2         = body2 \-\ [(v2, freshVar)]
    (gen, ns2)         = generalize ns1 freshBody1 freshBody2

generalize ns (Case sel1 bs1) (Case sel2 bs2) = (Generalization eGen theta' theta'', ns2) where
  (Generalization genSel sub' sub'', ns1) = generalize ns sel1 sel2
  (pats, ns2) = makeFresh ns1 bs1 bs2
  f (pat, Generalization e _ _) = (pat, e)
  bsGen   = map f pats
  theta'  = sub' ++ (concat $ map (subst1 . snd) pats)
  theta'' = sub'' ++ (concat $ map (subst2 . snd) pats)
  eGen = Case genSel bsGen

generalize ns e1 e2 = trivialGen ns e1 e2

makeFresh :: NameSupply -> [(Pat, Expr)] -> [(Pat, Expr)] -> ([(Pat, Generalization)], NameSupply)
makeFresh ns [] [] = ([], ns)

makeFresh ns ((Pat n1 args1, e1):pats1) ((Pat n2 args2, e2):pats2) = (result, ns3) where
  (freshNames, ns1) = splitAt (length args1) ns
  freshVars = map Var freshNames
  (genE, ns2) = generalize ns1 (e1 \-\ (zip args1 freshVars)) (e2 \-\ (zip args2 freshVars)) 
  (pats, ns3) = makeFresh ns2 pats1 pats2
  result = (Pat n1 freshNames, genE):pats
  

-- Homeomorphic embedding

embedding :: Expr -> Expr -> Bool
embedding (Var x) (Var y) = True
embedding (GlobRef x) (GlobRef y) = x == y
embedding e1 e2 = diving e1 e2 || coupling e1 e2 

coupling :: Expr -> Expr -> Bool
coupling (Constr name1 args1) (Constr name2 args2) 
  | name1 == name2 = all (== True) $ map (uncurry embedding) $ zip args1 args2

coupling (Lam v1 e1) (Lam v2 e2) = coupling e1 e2

coupling (e1' :@: e2') (e1'' :@: e2'') = (coupling e1' e1'') && (embedding e2' e2'')

coupling (Case e' opts') (Case e'' opts'') = 
  (coupling e' e'') && (all (== True) $ map (\((Pat n' _, e1), (Pat n'' _, e2)) -> (n' == n'') && (embedding e1 e2)) $ zip opts' opts'')

coupling (Var _) (Var _) = True

coupling e1 e2 = e1 == e2

diving :: Expr -> Expr -> Bool
diving e (Constr _ args) = any (== True) $ map (embedding e) args
diving e (Lam v0 e0)     = embedding e e0
diving e (e1 :@: e2)     = (embedding e e1) || (embedding e e2)
diving e (Case e0 opts)  = (embedding e e0) || (any (== True) $ map (\(_, ei) -> embedding e ei) opts)
diving e1 e2             = False 
