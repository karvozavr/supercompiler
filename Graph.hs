module Graph where

import Language
import LanguageIO
import LanguageUtil

data Option = Option Name Pat

data Step a = 
    Edge a 
    | Fold Expr Renaming 
    | Stop 
    | Options [(Option, a)] 
    | Edges [a] 

data Graph a = Node a (Step (Graph a))


showGraph (Node a step) = showStep a step

getNode (Node x _) = x

isFold (Node x (Fold _ _)) = True 
isFold _ = False

showStep :: Expr -> Step (Graph Expr) -> [String]
showStep e (Edge a)  = [("\"" ++ show e ++ "\" -> \"" ++ (if isFold a then " " else "") ++ (show $ getNode a) ++ "\" [ label=\"Drive\" ] \n")] ++ (showGraph a)
showStep e (Fold a _) = ["\" " ++ show e ++ "\" -> \"" ++ show a ++ "\" [ label=\"Fold\" ] \n"]
showStep e (Stop)  = ["\n"]
showStep e (Edges edges) = (map (\ex -> "\"" ++ show e ++ "\" -> \"" ++ (show $ getNode ex) ++ "\" [ label=\"Edges\" ] \n") edges) ++ (concat $ map (\x -> showGraph x) edges)
showStep e (Options edges) = (map (\ex -> "\"" ++ show e ++ "\" -> \"" ++ (if isFold ex then " " else "") ++ (show $ getNode ex) ++ "\" [ label=\"Option\" ] \n") $ map snd edges) ++ (concat $ map (\x -> showGraph x) (map snd edges))


renderDot graph = "digraph G {\n" ++ concat (showGraph graph) ++ "}"
    
reduceDrive :: Graph Expr -> Graph Expr
reduceDrive (Node ex step) = helper ex step where 
    helper ex (Edge n) = if (isFunSubst ex || isLet ex) then Node ex (Edge $ reduceDrive n) else reduceDrive n 
    helper ex f@(Fold e _) = Node ex f 
    helper ex Stop = Node ex Stop
    helper ex (Options edges) = Node ex $ Options $ map (\(o, n) -> (o, reduceDrive n)) edges

isLet (Let _ _) = True
isLet _ = False