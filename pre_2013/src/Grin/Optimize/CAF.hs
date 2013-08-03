{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleContexts, PatternGuards #-}
module Grin.Optimize.CAF
    ( optimize
    ) where

import Grin.Types

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Traverse


optimize :: Grin -> Grin
optimize = hoistPartialCAFs


{-
CAFs that evaluate to partial functions makes the job a lot harder for the HPT analyzer.
We can speed things up by eliminating such CAFs. They can all be replaced with a static
structure and a helper functions.

some_caf =
  do x <- store ...
     unit (P1other_function x)

x <- @eval some_caf
@apply x y

store (Tag some_caf)

====>

some_caf =
  do x <- store ...
     unit (P1other_function x)
new_caf =
  do s <- some_caf
     case s of
       P1other_function x -> unit (Structure x)
new_call =
  do s <- @eval new_caf
     case s of
       Structure x -> unit (P1other_function x)

x <- new_call
@apply x y

helperTag <- store (Fnew_call)
store (Tag helperTag)

-}
hoistPartialCAFs :: Grin -> Grin
hoistPartialCAFs grin
    | null candidates
    = grin
    | otherwise
    = case runState action (grinUnique grin) of
        (grin, unique') -> grin{grinUnique = unique'}
    where action     = do wrappers <- mapM mkCandidateWrappers candidates -- :: Candidate -> M (CAF, Node, FuncDef, FuncDef)
                          let env = Map.fromList [ (candidateCAF candidate, funcDefName unwrapperDef)
                                                 | (candidate, (caf, node, wrapperDef, unwrapperDef)) <- zip candidates wrappers ]
                              cafs = [ caf | (caf, _node, _wrapperDef, _unwrapperDef) <- wrappers ]
                              nodes = [ node | (_caf, node, _wrapperDef, _unwrapperDef) <- wrappers ]
                              wrapperFuncs = concat [ [wrapperDef, unwrapperDef] | (_caf, _node, wrapperDef, unwrapperDef) <- wrappers ]
                          funcs' <- mapM (updateCalls env) (grinFunctions grin)
                          return $ grin { grinFunctions = funcs' ++ wrapperFuncs
                                        , grinCAFs = grinCAFs grin ++ cafs
                                        , grinNodes = grinNodes grin ++ nodes }
          candidates = findCandidates grin



type M = State Int

newUnique :: M Int
newUnique
    = do u <- get
         put (u+1)
         return u

newVariable :: M Renamed
newVariable
    = do u <- newUnique
         return $ Anonymous u

newVariableFrom :: Renamed -> M Renamed
newVariableFrom oldName
    = do u <- newUnique
         case oldName of
           Aliased _uid alias -> return $ Aliased u alias
           _                  -> return $ Anonymous u

updateCalls :: Map.Map Renamed Renamed -> FuncDef -> M FuncDef
updateCalls env func
    = do body <- worker (funcDefBody func)
         return func{funcDefBody = body}
    where worker (Application (Builtin "eval") [ptr]) | Just newName <- Map.lookup ptr env
                               = return $ Application newName []
          worker (Store size value) = updateValue (Store size) value
          worker (Unit value)  = updateValue Unit value
          worker exp           = tmapM worker exp
          updateValue fn (Node _tag _type _missingArity args)
              = let loop acc []
                        = return $ fn (Node _tag _type _missingArity (reverse acc))
                    loop acc (x:xs)
                        | Just newName <- Map.lookup x env
                        = do v <- newVariable
                             rest <- loop (v:acc) xs
                             return $ Store 0 (Node newName FunctionNode 0 []) :>>= v :-> rest
                        | otherwise
                        = loop (x:acc) xs
                in loop [] args
          updateValue fn value = return $ fn value


-- Yikes, this function is too unwieldy.
mkCandidateWrappers :: Candidate -> M (CAF, NodeDef, FuncDef, FuncDef)
mkCandidateWrappers candidate
    = do nName <- newVariableFrom (candidateFunc candidate)
         funcName <- newVariableFrom (candidateFunc candidate)
         unwrapName <- newVariableFrom (candidateFunc candidate)
         cName <- newVariableFrom (candidateCAF candidate)

         s <- newVariable
         sArgs <- replicateM (candidateTagArity candidate) newVariable
         let branchNode = Node (candidateTag candidate) FunctionNode (candidateFuncArity candidate) sArgs
             structBody = Application (candidateFunc candidate) [] :>>= s :->
                          Case s [ branchNode :> Unit (Node nName ConstructorNode 0 sArgs)]
             structFunc = FuncDef { funcDefName = funcName
                                  , funcDefArgs = []
                                  , funcDefBody = structBody }

         s' <- newVariable
         s'Args <- replicateM (candidateTagArity candidate) newVariable
         let unwrapNode = Node nName ConstructorNode 0 s'Args
             unwrapBody = Application (Builtin "eval") [cName] :>>= s' :->
                          Case s' [ unwrapNode :> Unit (Node (candidateTag candidate) FunctionNode (candidateFuncArity candidate) s'Args)]
             unwrapFunc = FuncDef { funcDefName = unwrapName
                                  , funcDefArgs = []
                                  , funcDefBody = unwrapBody }

         let caf = CAF { cafName  = cName
                       , cafValue = Node funcName FunctionNode 0 [] }
         let node = NodeDef { nodeName = nName
                            , nodeType = ConstructorNode
                            , nodeArgs = replicate (candidateTagArity candidate) PtrType }

         return (caf, node, structFunc, unwrapFunc)
         

data Candidate = Candidate { candidateCAF       :: Renamed -- Name of the CAF value.
                           , candidateFunc      :: Renamed -- Function that the CAF initially points to.
                           , candidateTag       :: Renamed -- The tag that the above mentioned function evaluates to.
                           , candidateTagArity  :: Int     -- The number of arguments to that tag.
                           , candidateFuncArity :: Int     -- The number of missing arguments for the tag.
                           }

findCandidates :: Grin -> [Candidate]
findCandidates grin
    = [ Candidate { candidateCAF       = cafName caf
                  , candidateFunc      = funcDefName func
                  , candidateTag       = tag
                  , candidateTagArity  = tagArity
                  , candidateFuncArity = funcArity
                  }
      | func <- grinFunctions grin
      , Just caf <- [Map.lookup (funcDefName func) cafMap]
      , Just (tag, tagArity, funcArity) <- [isPCAFCandidate func] ]
    where cafMap = Map.fromList [ (func, caf) | caf@CAF{cafValue = Node func FunctionNode 0 []} <- grinCAFs grin]

isPCAFCandidate :: FuncDef -> Maybe (Renamed, Int, Int) -- (Tag, TagArity, FuncArity)
isPCAFCandidate = worker . funcDefBody
    where worker exp =
            case exp of
              _ :>>= _ :-> e -> worker e
              _ :>> e        -> worker e
              Unit (Node tag FunctionNode n args) | n > 0
                             -> Just (tag, length args, n)
              Case v [cond :> branch]
                             -> worker branch
              _other         -> Nothing

