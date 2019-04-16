{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Interpret (interpret) where

import           Control.Monad.Reader
import           Data.Char
import           Data.IORef
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Maybe
import           Foreign.LibFFI
import           Foreign.Ptr
import           Language.Haskell.Crux
import           Language.Haskell.Scope            (QualifiedName (..))
import           Language.Haskell.TypeCheck
import           Language.Haskell.TypeCheck.Pretty (displayIO, pretty,
                                                    renderPretty)
import           System.IO
import           System.IO.Unsafe
import           System.Posix.DynamicLinker

interpret :: Module -> IO ()
interpret m =
    case mbStart of
      Nothing -> error "entrypoint not found"
      Just body -> do
        hSetBuffering stdin NoBuffering
        ret <- runIM $ define defs $ do
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

newtype Scope = Scope (Map Variable (HP Expr))

emptyScope :: Scope
emptyScope = Scope Map.empty

lookupScope :: Variable -> Scope -> Maybe (HP Expr)
lookupScope key (Scope m) = Map.lookup key m

insertScope :: Variable -> HP Expr -> Scope -> Scope
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

interleaveIM :: IM a -> IM a
interleaveIM (IM action) = IM $ ReaderT $ \env ->
  unsafeInterleaveIO (runReaderT action env)

-- Variable -> (Scope, HP Expr)

-- let x = y z
-- in (x,x)

bind :: Variable -> Expr -> IM a -> IM a
bind v e action = do
  -- liftIO $ putStrLn $ "Bind: " ++ show v
  ref <- liftIO $ newIORef e
  local (\env -> env{envScope = insertScope v ref (envScope env)}) action

bindMany :: [(Variable, Expr)] -> IM a -> IM a
bindMany []             = id
bindMany ((key,val):xs) = bind key val . bindMany xs

define :: [(Variable, Expr)] -> IM a -> IM a
define defs action = do
  let (vs, es) = unzip defs
  refs <- liftIO $ mapM newIORef es
  local (\env -> env{envBindings = Scope $ Map.fromList (zip vs refs)}) action

requireHP :: Variable -> IM (HP Expr)
requireHP var = do
  Env{..} <- ask
  case listToMaybe (mapMaybe (lookupScope var) [envScope, envBindings]) of
    Just val -> pure val
    Nothing  -> error $ "var not found: " ++ show var

withScope :: Scope -> IM a -> IM a
withScope scope = local (\env -> env{envScope = scope})

deepSeq :: Expr -> IM Expr
deepSeq e = do
  e' <- eval e
  case e' of
    App a b -> do
      App a <$> deepSeq b
    _ -> pure e'

eval :: Expr -> IM Expr
eval e = do
  case e of
    Var v -> do
      hp <- requireHP v
      withScope emptyScope $ do
        e' <- liftIO $ readIORef hp
        liftIO $ writeIORef hp (Con $ blackhole v)
        v <- eval e'
        liftIO $ writeIORef hp v
        return v
    App fn a -> do
      a' <- lazyEval a
      fn' <- eval fn
      case fn' of
        Lam (x:xs) body ->
          eval (Lam xs $ Let (NonRec x a') body)
        _ -> pure (App fn' a')
    Case scrut key mbDef alts -> do
      scrut' <- eval scrut
      bind key scrut' $
        matchAlts mbDef scrut' alts
    Let (NonRec key val) body -> do
      val' <- lazyEval val
      bind key val' $ eval body
    -- ret, sOut, fn, args, sIn, body
    WithExternal ret sOut fn args _sIn body ->
      bind sOut (Lit LitVoid) $
      do fnPtr <- liftIO $ dlsym Default fn
         args' <- mapM eval args
         cRet <- liftIO $ callFFI fnPtr (toFFIRet $ varType ret) (map toFFIArg args')
         bind ret cRet $ eval body
    ExternalPure ret "indexI8#" [ptr] body -> do
      Lit (LitString str) <- eval ptr
      case str of
        [] ->
          bind ret (Lit $ LitWord 0) $
            eval body
        (c:_) ->
          bind ret (Lit $ LitWord $ fromIntegral $ ord c) $
            eval body
    ExternalPure ret "addrAdd#" [ptr, n] body -> do
      Lit (LitString str) <- eval ptr
      Lit (LitInt offset) <- eval n
      bind ret (Lit $ LitString $ drop (fromIntegral offset) str) $
        eval body
    Lam [] body -> eval body
    Convert body ty -> do
      body' <- eval body
      pure body'
    Lam{} -> pure e
    Con{} -> pure e
    Lit{} -> pure e
    UnboxedTuple es -> UnboxedTuple <$> mapM lazyEval es
    _ -> error $ "Unhandled expr: " ++ show e

lazyEval :: Expr -> IM Expr
lazyEval = interleaveIM . eval

matchAlts :: Maybe Expr -> Expr -> [Alt] -> IM Expr
matchAlts mbDef val alts = go alts
  where
    go [] =
      case mbDef of
        Nothing  -> error "no match in case expression"
        Just def -> eval def
    go (Alt pattern body : alts) =
      case pattern of
        UnboxedPat vs
          | UnboxedTuple ts <- val, length vs == length ts ->
            bindMany (zip vs ts) $ eval body
        ConPat con1 vars
          | (Con con2, args) <- splitApp val []
          , varName con1 == varName con2, length vars == length args ->
            bindMany (zip vars args) $ eval body
        LitPat lit1
          | Lit lit2 <- val
          , lit1 `litEq` lit2 ->
            eval body
        _ -> go alts
    splitApp (App a b) args = splitApp a (b:args)
    splitApp e args         = (e, args)

    litEq (LitInt a) (LitWord b) = a==b
    litEq (LitWord a) (LitInt b) = a==b
    litEq (LitChar c) b = litEq (LitInt $ fromIntegral $ ord c) b
    litEq a (LitChar c) = litEq a (LitInt $ fromIntegral $ ord c)
    litEq a b = a==b

toFFIRet :: Type -> RetType Expr
toFFIRet (TyCon (QualifiedName "LHC.Prim" "I32")) =
  fmap (\w -> Lit $ LitWord $ fromIntegral w) retWord32

toFFIArg :: Expr -> Arg
toFFIArg (Lit (LitWord n)) = argWord (fromIntegral n)
toFFIArg (Lit (LitInt n)) = argInt (fromIntegral n)

blackhole (Variable n t) =
  Variable (Name [] "Blackhole" 0) t
