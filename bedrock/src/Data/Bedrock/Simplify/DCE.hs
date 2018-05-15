{-# LANGUAGE LambdaCase #-}
module Data.Bedrock.Simplify.DCE (deadCodeElimination) where

import Data.Bedrock
import Data.Graph
import Data.Maybe

deadCodeElimination :: Module -> Module
deadCodeElimination m =
    m{ functions =
        [ fn
        | fn <- functions m
        , fromJust (toVertex (fnName fn)) `elem` live ]
     , modForeigns =
        [ f
        | f <- modForeigns m
        , fromJust (toVertex $ mkForeign (foreignName f)) `elem` live ]
     , nodes =
        [ node
        | node@(NodeDefinition name _) <- nodes m
        , fromJust (toVertex name) `elem` live]
      }
  where
    (graph, _fromVertex, toVertex) = graphFromEdges (moduleGraph m)
    live = reachable graph (fromJust $ toVertex $ entryPoint m)

moduleGraph :: Module -> [(Name, Name, [Name])]
moduleGraph m =
    [ (name, name, [])
    | f <- modForeigns m
    , let name = mkForeign (foreignName f) ] ++
    [ (name, name, [])
    | NodeDefinition name _ <- nodes m ] ++
    [ (fnName fn, fnName fn, blockDependencies (fnBody fn))
    | fn <- functions m ]
  where
    blockDependencies =
      \case
        Case _ mbDefault alts ->
          maybe [] blockDependencies mbDefault ++ concatMap altDependencies alts
        Bind _ expr block -> exprDependencies expr ++ blockDependencies block
        TailCall fn _ -> [fn]
        _ -> []
    altDependencies (Alternative _ block) = blockDependencies block
    exprDependencies =
      \case
        Application fn _ -> [fn]
        CCall fn _ -> [mkForeign fn ]
        Catch a _ b _ -> [a,b]

        Store (ConstructorName cons _) _ -> [cons]
        Store (FunctionName fn _) _ -> [fn]
        FunctionPointer fn -> [fn]

        MkNode (ConstructorName cons _) _ -> [cons]
        MkNode (FunctionName fn _) _ -> [fn]
        _ -> []

mkForeign :: String -> Name
mkForeign str = Name ["foreign"] str 0
