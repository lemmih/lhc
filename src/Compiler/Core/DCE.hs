-- Dead Code Elimination
module Compiler.Core.DCE (deadCodeElimination) where

import Data.Bedrock (Name)
import Compiler.Core
import Data.List
import Data.Graph
import Data.Maybe

deadCodeElimination :: Name -> Module -> Module
deadCodeElimination entrypoint m =
    m{ coreDecls =
        [ Decl ty name body
        | Decl ty name body <- coreDecls m
        , fromJust (toVertex name) `elem` live ]
      }
  where
    (graph, fromVertex, toVertex) = graphFromEdges (moduleGraph m)
    live = reachable graph (fromJust $ toVertex entrypoint)

moduleGraph :: Module -> [(Name, Name, [Name])]
moduleGraph m =
    [ (name, name, exprDependencies expr)
    | Decl _ty name expr <- coreDecls m ]
  where
    exprDependencies expr =
      case expr of
        Var var -> [varName var]
        Con con -> [varName con]
        UnboxedTuple args -> concatMap exprDependencies args
        Lit{} -> []
        WithExternal _ret _external _args _st rest -> exprDependencies rest
        ExternalPure _ret _external _args rest -> exprDependencies rest
        App a b -> exprDependencies a ++ exprDependencies b
        Lam vars e -> exprDependencies e \\ map varName vars
        Let letBind e -> letBindDependencies letBind ++ exprDependencies e
        LetStrict var e1 e2 -> exprDependencies e1 ++ exprDependencies e2
        Case scrut _var defaultBranch alts ->
          exprDependencies scrut ++
          maybe [] exprDependencies defaultBranch ++
          concatMap alternativeDependencies alts
        Cast e _ty -> exprDependencies e
        Id -> []
        WithCoercion _Coercion e -> exprDependencies e
    alternativeDependencies (Alt _pattern branch) = exprDependencies branch
    letBindDependencies (NonRec _var e) = exprDependencies e
    letBindDependencies (Rec binds) = concatMap (exprDependencies . snd) binds
