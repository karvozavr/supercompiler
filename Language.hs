module Language where 

import Data.Data

type Name = String

infixl 5 :@:

data Expr = 
      Var Name                 -- Variable reference
    | Constr Name [Expr]
    | Lam Name Expr
    | Expr :@: Expr
    | FunCall Name [Expr] -- Builtin function call with arguments
    | GlobRef Name -- Global reference
    | Let (Name, Expr) Expr -- Name binding 
    | Case Expr [(Pat, Expr)]
    deriving (Eq)

data Pat = Pat Name [Name] deriving (Eq)

data Def = Def Name Expr deriving (Eq) 

data Program = Program [Name] Expr [Def] deriving (Eq) 


type Renaming = [(Name, Name)]
type Subst = [(Name, Expr)]