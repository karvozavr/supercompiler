module Language where 

import Data.Data

type Name = String

data Expr = 
      Var Name                 -- Variable reference
    | Constr Name [Expr]
    | Lambda Name Expr
    | FunCall Name [Expr] -- Function call with arguments
    | Expr :@: Expr
    | Let (Name, Expr) Expr    -- Name binding 
    | Case Expr [(Pat, Expr)]
    deriving (Eq)

data Pat = Pat Name [Name] deriving (Eq)

data FunDef = FunDef Name [Name] Expr deriving (Eq) 

data Program = Program [FunDef] deriving (Eq) 
