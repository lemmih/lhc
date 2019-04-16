{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Interpret (interpret) where

import           Control.Monad.Reader
import           Data.IORef
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import System.IO
import           Language.Haskell.Crux
import           Language.Haskell.TypeCheck
import           Language.Haskell.TypeCheck.Pretty  (displayIO, pretty,
                                                     renderPretty)
import System.Posix.DynamicLinker
import Foreign.Ptr
import Foreign.LibFFI

interpret :: Module -> IO ()
interpret m =
    case mbStart of
      Nothing -> error "entrypoint not found"
      Just body -> do
        ret <- runIM $ bindMany defs $ do
          eval body
        displayIO stdout (renderPretty 1 100 (pretty ret))
  where
    defs =
      [ (Variable name ty, body)
      | Declaration ty name body <- cruxDecls m ]
    mbStart = listToMaybe
      [ body
      | Declaration ty name body <- cruxDecls m, name == entrypoint ]

entrypoint = Name ["Main"] "entrypoint" 0

type HP a = IORef a

newtype Scope = Scope (Map Variable (Scope, HP Expr))

emptyScope :: Scope
emptyScope = Scope Map.empty

lookupScope :: Variable -> Scope -> Maybe (Scope, HP Expr)
lookupScope key (Scope m) = Map.lookup key m

insertScope :: Variable -> (Scope, HP Expr) -> Scope -> Scope
insertScope key val (Scope m) = Scope (Map.insert key val m)

data Env = Env
  { envScope    :: Scope
  , envBindings :: Scope }

newtype IM a = IM (ReaderT Env IO a)
  deriving (MonadIO, MonadReader Env, Monad, Applicative, Functor)

runIM :: IM a -> IO a
runIM (IM action) = runReaderT action env
  where
    env = Env emptyScope emptyScope

-- Variable -> (Scope, HP Expr)

-- let x = y z
-- in (x,x)

bind :: Variable -> Expr -> IM a -> IM a
bind v e action = do
  scope <- asks envScope
  ref <- liftIO $ newIORef e
  local (\env -> env{envScope = insertScope v (scope, ref) (envScope env)}) action

bindMany :: [(Variable, Expr)] -> IM a -> IM a
bindMany [] = id
bindMany ((key,val):xs) = bind key val . bindMany xs

requireHP :: Variable -> IM (Scope, HP Expr)
requireHP var = do
  Env{..} <- ask
  case listToMaybe (mapMaybe (lookupScope var) [envScope, envBindings]) of
    Just val -> pure val
    Nothing  -> error $ "var not found: " ++ show var

withScope :: Scope -> IM a -> IM a
withScope scope = local (\env -> env{envScope = scope})

-- whnf
eval :: Expr -> IM Expr
eval e = do
  case e of
    Var v -> do
      (scope, hp) <- requireHP v
      withScope scope $ do
        e' <- liftIO $ readIORef hp
        liftIO $ writeIORef hp (Con $ blackhole v)
        v <- eval e'
        liftIO $ writeIORef hp v
        return v
    App fn a -> do
      fn' <- eval fn
      case fn' of
        Lam (x:xs) body ->
          eval (Lam xs (Let (NonRec x a) body))
        _ -> pure (App fn' a)
    Case scrut key mbDef alts -> do
      scrut' <- eval scrut
      bind key scrut' $
        undefined
    Let (NonRec key val) body ->
      bind key val $ eval body
    -- WithExternal Variable Variable String [Expr] Expr Expr
    Lam [] body -> eval body
    Lam{} -> pure e
    Con{} -> pure e
    _ -> error $ "Unhandled expr: " ++ show e

blackhole (Variable n t) =
  Variable (Name [] "Blackhole" 0) t
