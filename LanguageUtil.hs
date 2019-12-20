module LanguageUtil where

import Language
import LanguageLib
import Data.List
import Data.Maybe

infixl 5 \-\

-- Substitution operator
(\-\) :: Expr -> Subst -> Expr
(Var x)             \-\ sub = maybe (Var x) id (lookup x sub)
(Constr name args)  \-\ sub = Constr name (map (\-\ sub) args)
(Lam name body)     \-\ sub = Lam name (body \-\ (filter (\(x, _) -> x /= name) sub))
(Let (x, e1) e2)    \-\ sub = Let (x, (e1 \-\ sub)) (e2 \-\ sub)
(Case e cases)      \-\ sub = Case (e \-\ sub) (map (\(p, ei) -> (p, ei \-\ sub)) cases)
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