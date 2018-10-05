{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Bedrock.StackLayout
  ( lower
  ) where

import           Control.Applicative    (Applicative, pure, (<$>), (<*>))
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import qualified Data.Set               as Set

import           Data.Bedrock
import           Data.Bedrock.Transform (freeVariables)

data Env = Env
    { envStackLayout :: [(Variable, Int)]
    }
newtype M a = M { unM :: ReaderT Env (State AvailableNamespace) a }
    deriving
        ( Monad, MonadReader Env, Applicative, Functor
        , MonadState AvailableNamespace )

lower :: Module -> Module
lower m = runM (lowerModule m)
  where
    runM action = evalState (runReaderT (unM action) env) st
    st = modNamespace m
    env = Env
        { envStackLayout = [] }

lowerModule :: Module -> M Module
lowerModule m = do
    fns <- mapM lowerFunction (functions m)
    ns <- get
    return m{ functions = fns, modNamespace = ns }

lowerFunction :: Function -> M Function
lowerFunction fn = do
    body <- lowerBlock (fnBody fn)
    return fn{fnBody = body}

lowerBlock :: Block -> M Block
lowerBlock block =
    case block of
      Bind binds expr@Application{} rest -> do
        let free = Set.toList (freeVariables rest) \\ binds
        trimStackMap free $
          allotStackPositions free $ \mapped ->
          saveVariables mapped $
          Bind binds expr <$>
          (restoreVariables mapped $ lowerBlock rest)
      Bind binds expr rest ->
        Bind binds expr <$> lowerBlock rest
      Recursive binds rest ->
        Recursive binds <$> lowerBlock rest
      Case scrut defaultBranch alts ->
        Case scrut
          <$> maybe (return Nothing) (fmap Just . lowerBlock) defaultBranch
          <*> mapM lowerAlternative alts
      TailCall fn args -> pure $ TailCall fn args
      Exit -> pure Exit
      Panic msg -> pure $ Panic msg
      Return vars -> pure $ Return vars
      Raise{} -> pure block
      Invoke{} -> pure block
      -- InvokeHandler{} -> pure block

lowerAlternative :: Alternative -> M Alternative
lowerAlternative (Alternative pattern branch) =
    Alternative pattern <$> lowerBlock branch


--------------------------------------------------------
-- Utils

trimStackMap :: [Variable] -> M a -> M a
trimStackMap free = local $ \env -> env
  { envStackLayout = [ (var,idx) | (var,idx) <- envStackLayout env, var `elem` free ] }

allotStackPositions :: [Variable] -> ([(Variable, Int)] -> M a) -> M a
allotStackPositions free action = do
    action (zip (prims ++ pointers) freeSpots)
  where
    (prims, pointers) = partition isPrimitive free
    freeSpots = [2..]
    -- Sigh, this should be moved or refactored
    isPrimitive :: Variable -> Bool
    isPrimitive var =
      case variableType var of
        NodePtr{}    -> False
        Node{}       -> False
        StaticNode{} -> False
        IWord{}      -> True
        Primitive{}  -> True
        FramePtr     -> False


-- This implementation doesn't keep the invariant that primitives should come
-- before heap pointers.
-- allotStackPositions :: [Variable] -> ([(Variable, Int)] -> M a) -> M a
-- allotStackPositions free action = do
--     layout <- asks envStackLayout
--     let alreadyAllotted = map fst layout
--         -- The two first stack slots are reserved for the return address
--         -- and the previous stack frame
--         freeSpots = [2..] \\ map snd layout
--         newAllotted = zip (free \\ alreadyAllotted) freeSpots
--     local (\env -> env{ envStackLayout = sortBy (comparing snd) $ newAllotted ++ envStackLayout env }) $
--       action newAllotted

saveVariables :: [(Variable, Int)] -> M Block -> M Block
saveVariables [] fn           = fn
saveVariables ((var,n):xs) fn = Bind [] (Save var n) <$> saveVariables xs fn

restoreVariables :: [(Variable,Int)] -> M Block -> M Block
restoreVariables [] fn = fn
restoreVariables ((var,n):xs) fn = Bind [var] (Restore n) <$> restoreVariables xs fn
