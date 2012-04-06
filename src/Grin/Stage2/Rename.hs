module Grin.Stage2.Rename
    ( rename
    ) where

import Control.Monad.State.Strict
import qualified Data.Map as Map

import Grin.Stage2.Types

rename :: Grin -> Grin
rename grin = evalState (renameGrin grin) emptyState
    where emptyState = S { stateUnique = 1
                         , stateSubst  = Map.empty }


type M a = State S a
-- The substitution map could just as easily be a reader.
data S = S { stateUnique :: !Int
           , stateSubst  :: Map.Map Renamed Renamed
           }

renameGrin :: Grin -> M Grin
renameGrin grin
    = do mapM_ (bind . nodeName) (grinNodes grin)
         mapM_ (bind . cafName) (grinCAFs grin)
         forM_ (grinFunctions grin) $ \func -> do bind (funcDefName func)
                                                  mapM_ bind (funcDefArgs func)
         defs <- mapM renameFuncDef (grinFunctions grin)
         nodes <- mapM renameNode (grinNodes grin)
         cafs <- mapM renameCAF (grinCAFs grin)
         entryPoint <- newName (grinEntryPoint grin)
         newUnique <- gets stateUnique
         return Grin { grinNodes = nodes
                     , grinCAFs  = cafs
                     , grinFunctions = defs
                     , grinEntryPoint = entryPoint
                     , grinUnique = newUnique }

renameFuncDef :: FuncDef -> M FuncDef
renameFuncDef def
    = do name <- newName (funcDefName def)
         args <- mapM newName (funcDefArgs def)
         body <- renameExpression (funcDefBody def)
         return FuncDef{ funcDefName = name
                       , funcDefReturns = funcDefReturns def
                       , funcDefArgs = args
                       , funcDefBody = body }

renameExpression :: Expression -> M Expression
renameExpression (e1 :>>= binds :-> e2)
    = do e1' <- renameExpression e1
         binds' <- mapM bind binds
         e2' <- renameExpression e2
         return (e1' :>>= binds' :-> e2')
renameExpression (Application fn args)
    = liftM2 Application (newName fn) (mapM newName args)
renameExpression (Case scrut alts)
    = do scrut' <- newName scrut
         alts' <- mapM renameAlt alts
         return $ Case scrut' alts'
renameExpression (Fetch nth ptr)
    = liftM (Fetch nth) (newName ptr)
renameExpression (Store size args)
    = liftM (Store size) (mapM newName args)
renameExpression (StoreHole size)
    = return $ StoreHole size
renameExpression (Unit args)
    = liftM Unit (mapM newName args)
renameExpression (Constant val)
    = liftM Constant (renameValue val)

renameAlt :: Alt -> M Alt
renameAlt (cond :> branch)
    = do cond' <- renameValue cond
         branch' <- renameExpression branch
         return (cond' :> branch')

renameValue :: Value -> M Value
renameValue (Node node nt missing_args)
    = do node' <- newName node
         return $ Node node' nt missing_args
renameValue value = return value

renameNode :: NodeDef -> M NodeDef
renameNode node
    = do name <- newName (nodeName node)
         return node{nodeName = name}

renameCAF :: CAF -> M CAF
renameCAF caf
    = do name  <- newName (cafName caf)
         value <- renameValue (cafValue caf)
         return CAF{ cafName  = name
                   , cafValue = value }

bind :: Renamed -> M Renamed
bind name | isBuiltin name || isExternal name
    = return name
bind name
    = do uid <- newUnique
         s <- get
         when (name `Map.member` stateSubst s) $ error $ "Grin.Stage2.Rename.bind: Duplication of (supposedly) unique identifier: " ++ show name
         let newName = genNewName name uid
         put s { stateSubst = Map.insert name newName (stateSubst s) }
         return newName

newName :: Renamed -> M Renamed
newName oldName | isBuiltin oldName || isExternal oldName = return oldName
newName oldName@(Aliased (-1) _) = return oldName
newName oldName
    = do subst <- gets stateSubst
         case Map.lookup oldName subst of
           Nothing  -> error $ "Grin.Stage2.Rename.newName: Unbound variable: " ++ show oldName
           Just new -> return new

newUnique :: M Int
newUnique = do s <- get
               let unique = stateUnique s
               put s{stateUnique = unique + 1}
               return unique

genNewName (Aliased _uid name) uid = Aliased uid name
genNewName (Anonymous _uid) uid = Anonymous uid
genNewName other _uid = other
