module LanguageUtil(
    (\-\),
    isValue    
) where

import Language

infixl 5 \-\

-- Substitution operator
(\-\) :: Expr -> Subst -> Expr
(Var x)             \-\ sub = maybe (Var x) id (lookup x sub)
(Constr name args)  \-\ sub = Constr name (map (\-\ sub) args)
(Lam name body)     \-\ sub = Lam name (body \-\ (filter (\(x, _) -> x /= name) sub))
(FunCall name args) \-\ sub = FunCall name (map (\-\ sub) args)
(Let (x, e1) e2)    \-\ sub = Let (x, (e1 \-\ sub)) (e2 \-\ sub)
(Case e cases)      \-\ sub = Case (e \-\ sub) (map (\(p, ei) -> (p, ei \-\ sub)) cases)
(l :@: r)           \-\ sub = (l \-\ sub) :@: (r \-\ sub)

isValue :: Expr -> Bool
isValue (Constr _ args) = and $ map isValue args
isValue _ = False