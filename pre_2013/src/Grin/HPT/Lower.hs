{-# LANGUAGE OverloadedStrings #-}
module Grin.HPT.Lower
    ( lower
    ) where

import Grin.Types as Grin

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.List (delete)


import Grin.HPT.Environment (Lhs(..))
import Grin.HPT.Interface as Interface

import Config

data Env = Env { envOptConfig       :: OptConfig
               , envUpdateFunctions :: Map.Map Renamed Renamed
               }
type M a = ReaderT OptConfig (State (HeapAnalysis, Int)) a

lower :: OptConfig -> HeapAnalysis -> Grin -> (Grin, HeapAnalysis)
lower cfg hpt grin
    = case runState (runReaderT worker cfg) (hpt, grinUnique grin) of
        (grin, (hpt',_newUnique)) -> (grin, hpt')
    where worker = do fns <- mapM lowerFuncDef (grinFunctions grin)
                      unique <- gets snd
                      return grin{ grinFunctions = fns
                                 , grinUnique    = unique }

--addUpdateFunctions :: Grin -> M a -> M a
--addUpdateFunctions grin
--    = 

lowerFuncDef :: FuncDef -> M FuncDef
lowerFuncDef func
    = do body <- lowerExpression (funcDefBody func)
         return $ func{funcDefBody = body}

lowerExpression :: Expression -> M Expression
lowerExpression (a :>>= lam)
    = do a' <- lowerExpression a
         lam' <- lowerLambda lam
         return $ a' :>>= lam'
lowerExpression (a :>> b)
    = do a' <- lowerExpression a
         b' <- lowerExpression b
         return $ a' :>> b'
lowerExpression (Application (Builtin "eval") [a])
    = do f <- newVariable
         hpt <- gets fst
         case lookupHeap a hpt of
           Interface.Empty -> return $ Application (Builtin "unreachable") []
           rhs@Other{rhsTagged = nodes}
             -> do let tags = Map.toList nodes
                   addHPTInfo (VarEntry f) rhs -- (Tagged nodes)
                   alts <- mapM (mkApplyAlt f []) tags
                   v <- newVariable
                   let expand ((tag,FunctionNode,0),_args) = lookupLhs (VarEntry tag) hpt
                       expand (node,args) = Other (Map.singleton node args) []
                       expanded = mconcat $ map expand tags
                   addHPTInfo (VarEntry v) expanded
                   let anyShared = heapIsShared a hpt
                   u <- mkUpdate anyShared a f v tags
                   return $ Application (Builtin "fetch") [a] :>>= f :->
                            Case f alts :>>= v :->
                            u :>>
                            Unit (Variable v)
lowerExpression (Application (Builtin "apply") [a,b])
    = do hpt <- gets fst
         case lookupLhs (VarEntry a) hpt of
           Other{rhsTagged = nodes} -> do alts <- mapM (mkApplyAlt a [b]) (Map.toList nodes)
                                          return $ Case a alts
           Interface.Empty -> return $ Application (Builtin "unreachable") []
lowerExpression (Application (Builtin "caseUpdate") [ptr, val])
    = do hpt <- gets fst
         case lookupLhs (VarEntry val) hpt of
           Interface.Empty -> return $ Application (Builtin "unreachable") []
           rhs@Other{rhsTagged = nodes}
             -> do v <- newVariable
                   if heapIsShared ptr hpt
                     then return $ Unit Grin.Empty
                     else expandUpdate ptr val nodes
lowerExpression (Application fn args)
    = return $ Application fn args
lowerExpression (Update size ptr val)
    = return $ Update size ptr val
lowerExpression (Case scrut alts)
    = do hpt <- gets fst
         let rhs = lookupLhs (VarEntry scrut) hpt
         alts' <- mapM lowerAlt (filter (`isMemberOf` rhs) alts)
         return $ Case scrut alts'
lowerExpression (Store _ val)
    = do nodeSizeLimit <- asks optSmallNodeSize
         case val of
           Node _tag nt missingArity args -> return $ nodeToStore nodeSizeLimit nt missingArity (length args)
           Variable var
             -> do hpt <- gets fst
                   let rhs = lookupLhs (VarEntry var) hpt
                   case rhs of
                     Interface.Empty -> return $ Unit (Variable (Builtin "undefined"))
                     Other{rhsTagged = nodes}
                       -> return $ Case var [ Node tag nt missingArity [] :> nodeToStore nodeSizeLimit nt missingArity (length args) | ((tag, nt, missingArity), args) <- Map.toList nodes ]
           Hole size -> return $ Store size val
           Grin.Empty     -> return $ Store 1 val
    where nodeToStore nodeSizeLimit FunctionNode 0 nArgs
              = Store (max nodeSizeLimit (nArgs + 1)) val
          nodeToStore _nodeSizeLimit _nt _missingArity nArgs
              = Store (nArgs + 1) val
lowerExpression (Unit val) = return $ Unit val

lowerLambda :: Lambda -> M Lambda
lowerLambda (a :-> b)
    = do b' <- lowerExpression b
         return $ a :-> b'

lowerAlt :: Alt -> M Alt
lowerAlt (a :> b)
    = do b' <- lowerExpression b
         return $ a :> b'

(Node tag nt missing args :> _) `isMemberOf` (Other{rhsTagged = nodes})
    = (tag, nt, missing) `Map.member` nodes
_ `isMemberOf` rhs = True


{-
This function creates a set of update for a given pointer/value pair.
Updates should only be emitted when the pointer is shared (that is, used more than once),
and update should only be emitted when the pointer previously pointed to a suspended function.
-}
mkUpdate :: Bool -> Renamed -> Renamed -> Renamed ->[(Node, [Rhs])] -> M Expression
mkUpdate isShared ptr scrut val tags
    | not isShared
    = return $ Unit Grin.Empty
    | isShared
    = do hpt <- gets fst
         let doUpdate tag = case lookupLhs (VarEntry tag) hpt of
                              Other{rhsTagged = expanded} -> expandUpdate ptr val expanded
                              _                           -> return $ Unit Grin.Empty
         let worker ((tag, FunctionNode, 0), args)
                 = do args' <- replicateM (length args) newVariable
                      u <- doUpdate tag
                      return $ Node tag FunctionNode 0 args' :> u
             worker ((tag, nt, missingArgs), args)
                 = do args' <- replicateM (length args) newVariable
                      return $ Node tag nt missingArgs args' :> Unit Grin.Empty
         alts' <- mapM worker tags
         return $ Case scrut alts'


{-
We need to know the tag (to know the size) of a value before we can store it in memory.
This call takes a memory pointer, value identifier and a list of possible tags for the value.
Then a suitable update instruction is emitted for each tag. It could look like this:

case valueIdentifier of
  TagSizeThree _ _ -> update 3 pointerIdentifier valueIdentifier
  TagSizeOne       -> update 1 pointerIdentifier valueIdentifier
  TagSizeTwo _ _   -> update 2 pointerIdentifier valueIdentifier
-}
expandUpdate ptrIdent valueIdent tags
    = do nodeSizeLimit <- asks optSmallNodeSize
         if maxSize+1 <= nodeSizeLimit
            then return $ Update (maxSize+1) ptrIdent valueIdent
            else do alts <- mapM worker (Map.toList tags)
                    return $ Case valueIdent alts
    where worker ((tag, nt, missingArity), args)
              = do args' <- replicateM (length args) newVariable
                   node <- newVariable
                   addHPTInfo (VarEntry node) (Other (Map.singleton (tag, nt, missingArity) args) [])
                   return $ Node tag nt missingArity args' :> Update (length args' + 1) ptrIdent valueIdent
          maxSize = maximum [ length args | (_, args) <- Map.toList tags ]

mkApplyAlt :: Renamed -> [Renamed] -> (Node, [Rhs]) -> M Alt
mkApplyAlt scrut extraArgs ((tag, FunctionNode, n), argsRhs) | n == length extraArgs
    = do args <- replicateM (length argsRhs) newVariable
         return $ Node tag FunctionNode n args :> Application tag (args ++ extraArgs)
mkApplyAlt scrut [extraArg] ((tag, nt, 0), argsRhs)
    = return $ Node tag nt 0 [] :> Application (Builtin "unreachable") []
mkApplyAlt scrut [] ((tag, nt, n), argsRhs)
    = do args <- replicateM (length argsRhs) newVariable
         return $ Node tag nt n args :> Unit (Variable scrut)
mkApplyAlt scrut extraArgs ((tag, nt, n), argsRhs)
    = do args <- replicateM (length argsRhs) newVariable
         return $ Node tag nt n args :> Unit (Node tag nt (n - length extraArgs) (args ++ extraArgs))
mkApplyAlt _ _ val = error $ "Grin.HPT.Lower.mkApplyAlt: unexpected tag: " ++ show val

addHPTInfo :: Lhs -> Rhs -> M ()
addHPTInfo lhs rhs
    = modify $ \(hpt, unique) -> (hptAddBinding lhs rhs hpt, unique)

newVariable :: M Renamed
newVariable = do unique <- gets snd
                 modify $ \st -> (fst st, unique + 1)
                 return $ Anonymous unique

