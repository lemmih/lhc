-- Dead Code Elimination
module Language.Haskell.Crux.DCE (deadCodeElimination) where

import           Language.Haskell.Crux
import           Data.Graph
import           Data.List
import           Data.Maybe

deadCodeElimination :: Name -> Module -> Module
deadCodeElimination entrypoint m =
    m{ cruxDecls =
        [ Declaration ty name body
        | Declaration ty name body <- cruxDecls m
        , fromJust (toVertex name) `elem` live ]
      }
  where
    (graph, _fromVertex, toVertex) = graphFromEdges (moduleGraph m)
    live = reachable graph (fromJust $ toVertex entrypoint)

moduleGraph :: Module -> [(Name, Name, [Name])]
moduleGraph m =
    [ (name, name, exprDependencies expr)
    | Declaration _ty name expr <- cruxDecls m ]
  where
    exprDependencies expr =
      case expr of
        Var var -> [varName var]
        Con con -> [varName con]
        UnboxedTuple args -> concatMap exprDependencies args
        Lit{} -> []
        WithExternal _ret _retS _external _args _st rest -> exprDependencies rest
        ExternalPure _ret _external _args rest -> exprDependencies rest
        App a b -> exprDependencies a ++ exprDependencies b
        Lam vars e -> exprDependencies e \\ map varName vars
        Let letBind e -> letBindDependencies letBind ++ exprDependencies e
        LetStrict _var e1 e2 -> exprDependencies e1 ++ exprDependencies e2
        Case scrut _var defaultBranch alts ->
          exprDependencies scrut ++
          maybe [] exprDependencies defaultBranch ++
          concatMap alternativeDependencies alts
        Cast e _ty -> exprDependencies e
        Id -> []
        WithProof _p e -> exprDependencies e
        -- WithCoercion _Coercion e -> exprDependencies e
    alternativeDependencies (Alt _pattern branch) = exprDependencies branch
    letBindDependencies (NonRec _var e) = exprDependencies e
    letBindDependencies (Rec binds) = concatMap (exprDependencies . snd) binds
