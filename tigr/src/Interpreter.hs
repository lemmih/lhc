module Interpreter where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Data.IORef

type Name = String

type Context = Map Name (IORef Closure)
data Closure = Closure Context GCode

data Whnf
  = WhnfVar Name [IORef Closure]
  | WhnfCon Name [IORef Closure]
  | WhnfLit Int
  | WhnfClosure Closure

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

lookupName :: Name -> M (IORef Closure)
lookupName name = do
  st <- get
  case Map.lookup name st of
    Nothing      -> error $ "Missing name: " ++ name
    Just closure -> pure closure

lookupWhnf :: Name -> M Closure
lookupWhnf name = do
  ref <- lookupName name
  c <- liftIO $ readIORef ref
  c' <- runWithClosure c evaluate
  liftIO $ writeIORef ref c'
  return c'

toClosure :: [Name] -> GCode -> M Closure
toClosure vars code = do
  closures <- mapM lookupName vars
  return $ Closure (Map.fromList $ zip vars closures) code

runWithClosure :: Closure -> (GCode -> M a) -> M a
runWithClosure (Closure bound gcode) fn = do
  st <- get
  put bound
  val <- fn gcode
  put st
  return val

evaluate :: GCode -> M Closure
evaluate gcode =
  case gcode of
    Var "expensive#" [] -> toClosure [] (Lit 0)
    Var fn args -> do
      Closure ctx fn' <- lookupWhnf fn
      apply (toClosure (fn:args) gcode) fn' args
    Con _con args -> toClosure args gcode
    Let bind free rhs body -> do
      c <- liftIO . newIORef =<< toClosure free rhs
      modify $ Map.insert bind c
      evaluate body
    Lit{} -> toClosure [] gcode
    Case scrut (Just def) [] -> do
      Closure ctx scrut' <- lookupWhnf scrut
      evaluate def
    Lam free _bound _body -> toClosure free gcode

apply :: M Closure -> GCode -> [Name] -> M Closure
apply susp fn args = do
  case fn of
    Con con conArgs -> evaluate (Con con (conArgs ++ args))
    Lam free bound body
      | length args < length bound -> susp
      | length args == length bound -> do
        args' <- mapM lookupName args
        free' <- mapM lookupName free
        modify $ const $ Map.fromList (zip bound args' ++ zip free free')
        evaluate body
      | length args > length bound -> do
        args' <- mapM lookupName (take (length bound) args)
        args'' <- mapM lookupName (drop (length bound) args)
        free' <- mapM lookupName free
        modify $ const $ Map.fromList (zip bound args' ++ zip free free')
        Closure ctx newFn <- evaluate body
        modify $ const $ Map.union ctx (Map.fromList (zip (drop (length bound) args) args''))
        apply susp newFn (drop (length bound) args)
    _ -> error "Bad function application"

run :: GCode -> IO Closure
run gcode = evalStateT (evaluate gcode) Map.empty

printClosure :: Closure -> IO ()
printClosure = worker 0
  where
    worker i (Closure ctx gcode) = do
      forM_ (Map.toList ctx) $ \(name, ref) -> do
        putStrLn (replicate i ' ' ++ name ++ ":")
        worker (i+2) =<< liftIO (readIORef ref)
      putStrLn (replicate i ' '  ++ show gcode)

runTest :: GCode -> IO ()
runTest x = printClosure =<< run x

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
