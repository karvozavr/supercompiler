module LanguageLib where 

import Language

num :: Int -> Expr
num 0 = Constr "Z" []
num x = Constr "S" [num (x - 1)]

numFromChurch :: Expr -> Int
numFromChurch (Constr "Z" []) = 0
numFromChurch (Constr "S" [x]) = 1 + numFromChurch x

cons :: Expr -> Expr -> Expr
cons x xs = Constr "Cons" [x, xs] 

consPat :: Name -> Name -> Pat
consPat x xs = Pat "Cons" [x, xs]

nilPat :: Pat
nilPat = Pat "Nil" []

nil :: Expr
nil = Constr "Nil" []

plus :: Expr -> Expr -> Expr
plus x y = FunCall "+" [x, y]

mul :: Expr -> Expr -> Expr
mul x y = FunCall "*" [x, y]