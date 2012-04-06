{-# LANGUAGE OverloadedStrings #-}
module Grin.DeadCode
    ( removeDeadCode
    ) where

import Grin.Types

import qualified Data.Map as Map
import qualified Data.Set as Set


removeDeadCode :: Grin -> Grin
removeDeadCode grin
    = let dependenciesMap :: Map.Map Renamed (Set.Set Renamed)
          dependenciesMap = grinDepends grin
          loop seen keys
              = let deps = Set.unions [ find dependenciesMap key | key <- keys ]
                    newDeps = deps `Set.difference` seen
                in if Set.null newDeps
                   then seen
                   else loop (seen `Set.union` newDeps) (Set.toList newDeps)
          neededDependencies = Set.insert entryPoint $ loop Set.empty [entryPoint]
      in grin { grinFunctions = [ func | func <- grinFunctions grin, funcDefName func `Set.member` neededDependencies ]
              , grinCAFs      = [ caf  | caf  <- grinCAFs grin, cafName caf `Set.member` neededDependencies ]
              , grinNodes     = [ node | node <- grinNodes grin, nodeName node `Set.member` neededDependencies ]
              }
    where find m k = case Map.lookup k m of
                       Just v  -> v
                       Nothing -> error $ "Grin.DeadCode.removeDeadCode: Couldn't find key: " ++ show k
          entryPoint = grinEntryPoint grin


grinDepends :: Grin -> Map.Map Renamed (Set.Set Renamed)
grinDepends grin
    = Map.fromList [ (funcDefName def, defDepends def) | def <- grinFunctions grin ] `Map.union`
      Map.fromList [ (cafName caf, valueDepends (cafValue caf)) | caf <- grinCAFs grin ] `Map.union`
      Map.fromList [ (nodeName node, Set.empty) | node <- grinNodes grin ]

defDepends :: FuncDef -> Set.Set Renamed
defDepends def
    = expDepends (funcDefBody def) `Set.difference` Set.fromList (funcDefArgs def)

expDepends :: Expression -> Set.Set Renamed
expDepends (Store v) = valueDepends v
expDepends (Unit v)  = valueDepends v
expDepends (Update size ptr val)
    = Set.fromList [ptr,val]
expDepends (Application fn args) | isBuiltin fn
    = Set.fromList args
expDepends (Application fn args) | isExternal fn
    = Set.fromList args
expDepends (Application fn args)
    = Set.fromList (fn:args)
expDepends (Case scrut alts)
    = Set.insert scrut $ Set.unions (map altDepends alts)
expDepends (a :>> b)
    = expDepends a `Set.union` expDepends b
expDepends (a :>>= bind :-> b)
    = expDepends a `Set.union` Set.delete bind (expDepends b)

altDepends :: Alt -> Set.Set Renamed
altDepends (Node tag _nt _missing args :> branch)
    = Set.insert tag $ expDepends branch `Set.difference` Set.fromList args
altDepends (cond :> branch)
    = expDepends branch `Set.difference` valueDepends cond

valueDepends :: Value -> Set.Set Renamed
valueDepends (Node tag _nt _missing args)
    = Set.fromList (tag:args)
valueDepends (Vector args)
    = Set.fromList args
valueDepends Lit{} = Set.empty
valueDepends (Variable v) = Set.singleton v
valueDepends Hole{} = Set.empty
valueDepends Empty = Set.empty

