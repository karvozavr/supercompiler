module Graph where

import Language

data Option = Option Name Pat

data Step a = 
      Edge a 
    | Fold a Renaming 
    | Stop 
    | Options [(Option, a)] 
    | Edges [a]

data Graph a = Node a (Step (Graph a))
