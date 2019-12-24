module Graph where

import Language
import LanguageIO

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

showStep :: Expr -> Step (Graph Expr) -> [String]
showStep e (Edge a)  = [("\"" ++ show e ++ "\" -> \"" ++ (show $ getNode a) ++ "\" [ label=\"Drive\" ] \n")] ++ (showGraph a)
showStep e (Fold a _) = ["\"" ++ show e ++ "\" -> \"" ++ show a ++ "\" [ label=\"Fold\" ] \n"]
showStep e (Stop)  = ["\n"]
showStep e (Edges edges) = (map (\ex -> "\"" ++ show e ++ "\" -> \"" ++ (show $ getNode ex) ++ "\" [ label=\"Edges\" ] \n") edges) ++ (concat $ map (\x -> showGraph x) edges)
showStep e (Options edges) = (map (\ex -> "\"" ++ show e ++ "\" -> \"" ++ (show $ getNode ex) ++ "\" [ label=\"Option\" ] \n") $ map snd edges) ++ (concat $ map (\x -> showGraph x) (map snd edges))


renderDot graph = "digraph G {\n" ++ concat (showGraph graph) ++ "}"
    