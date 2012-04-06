module Grin.SimpleCore.DeadCode
    ( removeDeadCode
    ) where

import CompactString
import Grin.SimpleCore.Types

import qualified Data.Map as Map
import qualified Data.Set as Set



removeDeadCode :: [(String,String)] -> [String] -> Map.Map (String,String) SimpleModule -> (SimpleNewTypes, [SimpleType], [SimpleEnum], [SimpleDef])
removeDeadCode initialModules entryPoints modules
    = let entryPointsCompact = map fromString entryPoints
          addModules mods entries = entries `Map.union` Map.unions (map (entityMap `find`) mods)
          loop entries seenMods seen ds
              = let (mods,refs) = unzip (map (entries `find`) ds)
                    new  = Set.unions refs `Set.difference` seen
                    seen' = Set.union seen new
                in if Set.null new
                   then ( Set.toList seenMods
                        , seen `Set.union` Set.fromList entryPointsCompact)
                   else loop (addModules (concat mods) entries) (Set.union seenMods (Set.fromList (concat mods)))  seen' (Set.toList new)
          (modDeps, deps) = loop (addModules initialModules Map.empty) (Set.fromList initialModules) Set.empty entryPointsCompact
          neededMods = map (modules `find`) modDeps
          tdefs = concatMap moduleTypes neededMods
          defs = concatMap moduleDefs neededMods
      in ( Map.unions (map moduleNewTypes neededMods)
         , [ tdef | tdef <- tdefs] -- Unused nodes are removed later.
         , concatMap moduleEnums neededMods
         , [ def  | def  <- defs, simpleDefName def `Set.member` deps ]
         )
    where find m k = case Map.lookup k m of
                       Just v  -> v
                       Nothing -> error $ "Grin.SimpleCore.DeadCode.removeDeadCode: Couldn't find key: " ++ show k
          entityMap :: Map.Map (String,String) (Map.Map CompactString ([(String,String)], Set.Set CompactString)) 
          entityMap = flip Map.map modules $ \smod ->
                      Map.fromList $ [ (simpleDefName def, (simpleDefDeps def, defDependencies def)) | def <- moduleDefs smod ] ++
                                     [ (simpleTypeName tdef, ([],Set.empty)) | tdef <- moduleTypes smod]


defDependencies :: SimpleDef  -> Set.Set CompactString
defDependencies def
    = dependencies (simpleDefBody def) `Set.difference` Set.fromList (simpleDefArgs def)

dependencies :: SimpleExp -> Set.Set CompactString
dependencies (Var var isUnboxed) = Set.singleton var
dependencies Primitive{}= Set.empty
dependencies (EnumPrimitive prim arg ty) = Set.singleton arg
dependencies (Dcon var) = Set.singleton var
dependencies Lit{} = Set.empty
dependencies (App a ty_args args) = Set.unions (dependencies a : map dependencies args)
dependencies (Let var toplevel _ _ _ e)
    = Set.delete var $ Set.insert toplevel $ dependencies e
dependencies (LetRec defs e)
    = let vars      = Set.fromList [ var | (var,_,_,_,_) <- defs ]
          toplevels = Set.fromList [ toplevel | (_,toplevel,_,_,_) <- defs ]
      in (dependencies e `Set.union` toplevels) `Set.difference` vars
dependencies (LetStrict name def e)
    = Set.delete name $ dependencies def `Set.union` dependencies e
dependencies (Case e bind ty alts)
    = Set.delete bind $ Set.unions (dependencies e : map altDependencies alts)
dependencies (CaseStrict e bind ty alts)
    = Set.delete bind $ Set.unions (dependencies e : map altDependencies alts)
dependencies External{} = Set.empty
dependencies DynExternal{} = Set.empty
dependencies Label{} = Set.empty
dependencies (Note _ e) = dependencies e

altDependencies :: Alt -> Set.Set CompactString
altDependencies (Acon tag args e)
    = Set.insert tag $ dependencies e `Set.difference` Set.fromList args
altDependencies (Alit _ e)
    = dependencies e
altDependencies (Adefault e)
    = dependencies e

