module Interpreter where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Data.IORef

type Name = String

type Context = Map Name (IORef Thunk)
data Closure = Closure Context GCode

data Thunk
  = ThunkCon Name [IORef Thunk]
  | ThunkLit Int
  | ThunkClosure Closure

data GCode
  = Var Name [Name]
  | Con Name [Name]
  | Let Name [Name] GCode GCode
  | Lit Int
  | Case Name (Maybe GCode) [Alt]
  | Lam [Name] [Name] GCode
    deriving (Show)

data Alt = Alt Pattern GCode
  deriving (Show)

data Pattern
  = ConPattern Name [Name]
  | LitPattern Int
    deriving (Show)

type M a = StateT Context IO a

lookupName :: Name -> M (IORef Thunk)
lookupName name = do
  st <- get
  case Map.lookup name st of
    Nothing      -> error $ "Missing name: " ++ name
    Just closure -> pure closure

lookupWhnf :: Name -> M Thunk
lookupWhnf name = do
  ref <- lookupName name
  t <- liftIO $ readIORef ref
  t' <- case t of
          ThunkClosure c -> runWithClosure c evaluate
          _ -> pure t
  liftIO $ writeIORef ref t'
  return t'

toClosure :: [Name] -> GCode -> M Closure
toClosure vars code = do
  thunks <- mapM lookupName vars
  return $ Closure (Map.fromList $ zip vars thunks) code

runWithClosure :: Closure -> (GCode -> M a) -> M a
runWithClosure (Closure bound gcode) fn = do
  st <- get
  put bound
  val <- fn gcode
  put st
  return val

evaluate :: GCode -> M Thunk
evaluate gcode =
  case gcode of
    Var "expensive#" [] -> pure $ ThunkLit 0
    Var fn args -> do
      thunk <- lookupWhnf fn
      apply thunk =<< mapM lookupName args
    Con con args ->
      ThunkCon con <$> mapM lookupName args
    Let bind free rhs body -> do
      c <- toClosure free rhs
      thunk <- liftIO $ newIORef $ ThunkClosure c
      modify $ Map.insert bind thunk
      evaluate body
    Lit i -> pure $ ThunkLit i
    Case scrut (Just def) [] -> do
      _thunk <- lookupWhnf scrut
      evaluate def
    Lam free _bound _body -> ThunkClosure <$> toClosure free gcode

-- Invariant: thunk is in whnf
apply :: Thunk -> [IORef Thunk] -> M Thunk
apply thunk args =
  case thunk of
    ThunkCon con params -> do
      pure $ ThunkCon con (params ++ args)
    ThunkLit{} -> error "Lit apply?"
    ThunkClosure (Closure ctx gcode) ->
      case gcode of
        Lam free bound body | length bound == length args -> do
          put ctx
          free' <- mapM lookupName free
          put $ Map.fromList $ zip (bound++free) (args++free')
          evaluate body
        Lam free bound body | length bound > length args -> do
          put ctx
          free' <- mapM lookupName free
          pure $ ThunkClosure $ Closure (Map.fromList $ zip (bound++free) (args++free')) (Lam free (drop (length args) bound) body)
        Lam free bound body | length bound < length args -> do
          let (nowArgs, laterArgs) = splitAt (length bound) args
          put ctx
          free' <- mapM lookupName free
          put $ Map.fromList $ zip (bound++free) (nowArgs++free')
          thunk' <- evaluate body
          apply thunk' laterArgs
        _ -> error "Closure apply?"
-- apply :: M Closure -> GCode -> [Name] -> M Closure
-- apply susp fn args = do
--   case fn of
--     Con con conArgs -> evaluate (Con con (conArgs ++ args))
--     Lam free bound body
--       | length args < length bound -> susp
--       | length args == length bound -> do
--         args' <- mapM lookupName args
--         free' <- mapM lookupName free
--         modify $ const $ Map.fromList (zip bound args' ++ zip free free')
--         evaluate body
--       | length args > length bound -> do
--         args' <- mapM lookupName (take (length bound) args)
--         args'' <- mapM lookupName (drop (length bound) args)
--         free' <- mapM lookupName free
--         modify $ const $ Map.fromList (zip bound args' ++ zip free free')
--         Closure ctx newFn <- evaluate body
--         modify $ const $ Map.union ctx (Map.fromList (zip (drop (length bound) args) args''))
--         apply susp newFn (drop (length bound) args)
--     _ -> error "Bad function application"

run :: GCode -> IO Thunk
run gcode = evalStateT (evaluate gcode) Map.empty

printClosure :: Int -> Closure -> IO ()
printClosure i (Closure ctx gcode) = do
  forM_ (Map.toList ctx) $ \(name, ref) -> do
    putStrLn (replicate i ' ' ++ name ++ ":")
    printThunk (i+2) =<< liftIO (readIORef ref)
  putStrLn (replicate i ' '  ++ "Closure: " ++ show gcode)

printThunk :: Int -> Thunk -> IO ()
printThunk i thunk =
  case thunk of
    ThunkCon n refs -> do
      thunks <- mapM readIORef refs
      mapM (printThunk i) thunks
      putStrLn $ replicate i ' ' ++ "Con: " ++ n
    ThunkLit n -> putStrLn $ replicate i ' ' ++ show n
    ThunkClosure c -> printClosure i c

runTest :: GCode -> IO ()
runTest x = printThunk 0 =<< run x

test0 :: GCode
test0 =
  Let "y" [] (Lit 10) $
  Con "Just" ["y"]

test1 :: GCode
test1 =
  Let "x" [] (Con "Just" []) $
  Let "y" [] (Lit 10) $
  (Var "x" ["y"])

test2 :: GCode
test2 =
  Let "expensive" [] (Var "expensive#" []) $
  Let "a" ["expensive"] (Con "Just" ["expensive"]) $
  Let "b" ["expensive"] (Con "Just" ["expensive"]) $
  Con "Tuple" ["a","b"]

test3 :: GCode
test3 =
  Let "expensive" [] (Var "expensive#" []) $
  Let "a" ["expensive"] (Con "Just" ["expensive"]) $
  Let "b" ["expensive"] (Con "Just" ["expensive"]) $
  Case "expensive" (Just $
  Con "Tuple" ["a","b"]) []

test4 :: GCode
test4 =
  Let "fn" [] (Lam [] ["x"] $ Con "Just" ["x"]) $
  Var "fn" ["fn"]

test5 :: GCode
test5 =
  Let "fn" [] (Lam [] ["x"] $ Con "Just" ["x"]) $
  Let "i" [] (Lit 10) $
  Var "fn" ["i"]

test6 :: GCode
test6 =
  Let "fn" [] (Lam [] ["x", "y"] $ Con "Just" ["x", "y"]) $
  Let "i" [] (Lit 10) $
  Var "fn" ["i"]

test7 :: GCode
test7 =
  Let "fn" [] (Con "Just" []) $
  Let "i" [] (Lit 10) $
  Var "fn" ["i"]

test8 :: GCode
test8 =
  Let "fn" [] (Lam [] ["x"] $ Con "Just" []) $
  Let "i" [] (Lit 10) $
  Var "fn" ["i", "i"]

test9 :: GCode
test9 =
  Let "i" [] (Lit 10) $
  Let "fn" ["i"] (Con "Just" ["i"]) $
  Let "i" [] (Lit 12) $
  Var "fn" ["i"]
