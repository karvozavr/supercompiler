module LanguageLib where 

import Language

num :: Int -> Expr
num 0 = Constr "Z" []
num x = Constr "S" [num (x - 1)]

numFromChurch :: Expr -> Int
numFromChurch (Constr "Z" []) = 0
numFromChurch (Constr "S" [x]) = 1 + numFromChurch x

isNum :: Expr -> Bool
isNum (Constr "Z" []) = True
isNum (Constr "S" [x]) = isNum x
isNum _ = False

cons :: Expr -> Expr -> Expr
cons x xs = Constr "Cons" [x, xs] 

consPat :: Name -> Name -> Pat
consPat x xs = Pat "Cons" [x, xs]

nilPat :: Pat
nilPat = Pat "Nil" []

nil :: Expr
nil = Constr "Nil" []

bTrue = Constr "True" []
bFalse = Constr "False" []

bTrueP = Pat "True" []
bFalseP = Pat "False" []

plus :: Expr -> Expr -> Expr
plus x y = (GlobRef "+") :@: x :@: y

mul :: Expr -> Expr -> Expr
mul x y =  (GlobRef "*") :@: x :@: y

fromBool a = if a then bTrue else bFalse

builtinPlus [x, y] = num ((numFromChurch x) + (numFromChurch y))
builtinMul [x, y] = num ((numFromChurch x) * (numFromChurch y))
builtinLess [x, y] = fromBool ((numFromChurch x) < (numFromChurch y))
builtinMore [x, y] = fromBool ((numFromChurch x) > (numFromChurch y))

builtins :: [Def]
builtins = [
    Def "+" $ Lam "builtin_x" $ Lam "builtin_y" (Case (Var "builtin_x") [(Pat "Z" [], Var "builtin_y"), (Pat "S" ["builtin_x'"], GlobRef "+" :@: Var "builtin_x'" :@: (Constr "S" [Var "builtin_y"]))]),
    Def "*" $ Lam "builtin_x" $ Lam "builtin_y" (Case (Var "builtin_x") [(Pat "Z" [], Constr "Z" []), (Pat "S" ["builtin_x'"], plus (Var "builtin_y") (mul (Var "builtin_x'") (Var "builtin_y")))]), 
    Def ">" $ Lam "builtin_x" $ Lam "builtin_y" (Case (Var "builtin_x") 
        [(Pat "Z" [], bFalse), 
         (Pat "S" ["builtin_x'"], Case (Var "builtin_y") [(Pat "Z" [], bTrue), (Pat "S" ["builtin_y'"], GlobRef ">" :@: Var "builtin_x'" :@: Var "builtin_y'")]) ]) ]