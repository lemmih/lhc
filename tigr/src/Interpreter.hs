{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Interpreter where


import           Control.Exception    (Exception (..), SomeException (..),
                                       handle, throwIO)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.IORef
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Typeable

data CodeException = CodeException (IORef Thunk)
  deriving (Typeable)
instance Show CodeException where
  show _ = "CodeException"
instance Exception CodeException where
  toException = SomeException

type Name = String

type Context = Map Name (IORef Thunk)
data Closure = Closure Context GCode

data Thunk
  = ThunkCon Name [IORef Thunk]
  | ThunkLit Literal
  | ThunkClosure Closure

data Literal
  = LiteralI64 Int
  | LiteralString String
  deriving (Show, Eq)

{- Common patterns:
Var topLevelFn args
Let var [] (Lit lit) $ ...
LetStrict "" (Var builtin args) $ ...
LetStrict bind (Lit lit) $ ...
LetStrict bind (Con con args) $ ...
Case scrut_whnf
Case with all literals
Case with all constructors
Var CAF -> Var (IORef Thunk)
Var GlobalFn -> VarPartial GCode [Name] | VarComplete GCode [Name] | VarSuper GCode [Name]

GCode = Array of lets + endpoint (var,con,lit,case,lam)
-}
data GCode
  = Var Name [Name]
  | Con Name [Name]
  | External String [Name]
  | Let Name [Name] GCode GCode
  | LetRec [(Name, [Name], GCode)] GCode
  | LetStrict Name GCode GCode
  | Lit Literal
  | Case Name (Maybe GCode) [Alt]
  | Lam [Name] [Name] GCode
  | Throw Name
  | Catch [Name] Name GCode GCode
    deriving (Show)

data Alt = Alt Pattern GCode
  deriving (Show)

data Pattern
  = ConPattern Name [Name]
  | LitPattern Literal
    deriving (Show)

newtype M a = M { unM :: Context -> Context -> IO a }

instance Functor M where
  fmap f (M g) = M $ \global local -> fmap f (g global local)

instance Applicative M where
  pure a = M $ \_ _ -> pure a
  f <*> g = M $ \global local -> unM f global local <*> unM g global local

instance Monad M where
  f >>= g = M $ \global local -> unM f global local >>= \a -> unM (g a) global local

instance MonadIO M where
  liftIO io = M $ \_global _local -> io

askLocal :: M Context
askLocal = M $ \_global local -> pure local

askGlobal :: M Context
askGlobal = M $ \global _local -> pure global

withLocal :: Context -> M a -> M a
withLocal local f = M $ \global _local -> unM f global local

modifyLocal :: (Context -> Context) -> M a -> M a
modifyLocal m f = M $ \global local -> unM f global (m local)

lookupName :: Name -> M (IORef Thunk)
lookupName name = do
  local <- askLocal
  global <- askGlobal
  case Map.lookup name local of
    Nothing      ->
      case Map.lookup name global of
        Nothing -> error $ "Missing name: " ++ name
        Just closure -> pure closure
    Just closure -> pure closure

lookupWhnf :: Name -> M Thunk
lookupWhnf name = do
  ref <- lookupName name
  t <- liftIO $ readIORef ref
  case t of
    ThunkClosure c -> do
      liftIO $ writeIORef ref $ ThunkCon "Whitehole" []
      t' <- runWithClosure c evaluate
      liftIO $ writeIORef ref t'
      return t'
    ThunkCon "Whitehole" [] -> error "Whitehole loop"
    _ -> return t

toClosure :: [Name] -> GCode -> M Closure
toClosure vars code = do
  thunks <- mapM lookupName vars
  return $ Closure (Map.fromList $ zip vars thunks) code

runWithClosure :: Closure -> (GCode -> M a) -> M a
runWithClosure (Closure bound gcode) fn = withLocal bound (fn gcode)

handleM :: (CodeException -> M a) -> M a -> M a
handleM handler f = M $ \global local ->
  handle (\e -> unM (handler e) global Map.empty) (unM f global local)

evaluate :: GCode -> M Thunk
evaluate gcode =
  case gcode of
    Var "expensive#" [] -> pure $ ThunkLit $ LiteralI64 0
    Var "printLit#" [thunk] -> do
      ThunkLit lit <- lookupWhnf thunk
      liftIO $ print lit
      pure $ ThunkLit $ LiteralI64 0
    Var fn args -> do
      thunk <- lookupWhnf fn
      apply thunk =<< mapM lookupName args
    Con con args ->
      ThunkCon con <$> mapM lookupName args
    Let bind free rhs body -> do
      c <- toClosure free rhs
      thunk <- liftIO $ newIORef $ ThunkClosure c
      modifyLocal (Map.insert bind thunk)
        (evaluate body)
    LetRec binds body -> do
      let names = [ name | (name, _, _) <- binds ]
      refs <- liftIO $ replicateM (length binds) $ newIORef undefined
      modifyLocal (Map.union $ Map.fromList $ zip names refs) $ do
        forM_ (zip binds refs) $ \((name, free, rhs), ref) -> do
          c <- toClosure free rhs
          liftIO $ writeIORef ref $ ThunkClosure c
        evaluate body
    LetStrict bind rhs body -> do
      thunk_ <- evaluate rhs
      thunk <- liftIO $ newIORef thunk_
      modifyLocal (Map.insert bind thunk)
        (evaluate body)
    Lit i -> pure $ ThunkLit i
    Case scrut mbDef alts -> do
      thunk <- lookupWhnf scrut
      runCase thunk mbDef alts
    Lam free _bound _body -> ThunkClosure <$> toClosure free gcode
    Throw name -> do
      exception <- lookupName name
      liftIO $ throwIO $ CodeException exception
    Catch free bound handler body -> do
      free' <- mapM lookupName free
      let outerCtx = Map.fromList $ zip free free'
          helper (CodeException thunk) =
            withLocal (Map.insert bound thunk outerCtx) (evaluate handler)
      handleM helper (evaluate body)

runCase :: Thunk -> Maybe GCode -> [Alt] -> M Thunk
runCase thunk Nothing [] = error "Case fell through"
runCase thunk (Just def) [] = evaluate def
runCase thunk mbDef (Alt pattern branch : alts) =
  case (thunk, pattern) of
    (ThunkLit lit, LitPattern pLit) | lit == pLit -> evaluate branch
    (ThunkCon con args, ConPattern pCon pArgs) | con == pCon ->
      modifyLocal (Map.union (Map.fromList $ zip pArgs args))
        (evaluate branch)
    (ThunkCon{}, LitPattern{}) -> error "Con/Lit pattern match"
    (ThunkLit{}, ConPattern{}) -> error "Lit/Con pattern match"
    (ThunkClosure{}, _)        -> error "Closure pattern match"
    _ -> runCase thunk mbDef alts

-- Invariant: thunk is in whnf
apply :: Thunk -> [IORef Thunk] -> M Thunk
apply thunk args =
  case thunk of
    ThunkCon con params -> do
      pure $ ThunkCon con (params ++ args)
    ThunkLit{} -> error "Lit apply?"
    ThunkClosure (Closure ctx gcode) -> withLocal ctx $
      case gcode of
        Lam free bound body | length bound == length args ->
          modifyLocal (Map.union $ Map.fromList $ zip bound args)
            (evaluate body)
        Lam free bound body | length bound > length args ->
          pure $ ThunkClosure $ Closure (Map.union ctx $ Map.fromList $ zip bound args) (Lam free (drop (length args) bound) body)
        Lam free bound body | length bound < length args -> do
          let (nowArgs, laterArgs) = splitAt (length bound) args
          modifyLocal (Map.union $ Map.fromList $ zip bound nowArgs) $ do
            thunk' <- evaluate body
            apply thunk' laterArgs
        _ -> error "Closure apply?"

run :: GCode -> IO Thunk
run gcode = unM (evaluate gcode) Map.empty Map.empty

printClosure :: Int -> Closure -> IO ()
printClosure i _ | i > 10 = putStrLn (replicate i ' ' ++ "snip")
printClosure i (Closure ctx gcode) = do
  printContext i ctx
  putStrLn (replicate i ' '  ++ "Closure: " ++ show gcode)

printThunk :: Int -> Thunk -> IO ()
printThunk i _ | i > 10 = putStrLn (replicate i ' ' ++ "snip")
printThunk i thunk =
  case thunk of
    ThunkCon n refs -> do
      thunks <- mapM readIORef refs
      mapM (printThunk (i+2)) thunks
      putStrLn $ replicate i ' ' ++ "Con: " ++ n
    ThunkLit n -> putStrLn $ replicate i ' ' ++ show n
    ThunkClosure c -> printClosure i c

printContext :: Int -> Context -> IO ()
printContext i ctx = do
  forM_ (Map.toList ctx) $ \(name, ref) -> do
    putStrLn (replicate i ' ' ++ name ++ ":")
    printThunk (i+2) =<< liftIO (readIORef ref)

runTest :: GCode -> IO ()
runTest x = printThunk 0 =<< run x

test0 :: GCode
test0 =
  Let "y" [] (Lit $ LiteralI64 10) $
  Con "Just" ["y"]

test1 :: GCode
test1 =
  Let "x" [] (Con "Just" []) $
  Let "y" [] (Lit $ LiteralI64 10) $
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
  Let "i" [] (Lit $ LiteralI64 10) $
  Var "fn" ["i"]

test6 :: GCode
test6 =
  Let "fn" [] (Lam [] ["x", "y"] $ Con "Just" ["x", "y"]) $
  Let "i" [] (Lit $ LiteralI64 10) $
  Var "fn" ["i"]

test7 :: GCode
test7 =
  Let "fn" [] (Con "Just" []) $
  Let "i" [] (Lit $ LiteralI64 10) $
  Var "fn" ["i"]

test8 :: GCode
test8 =
  Let "fn" [] (Lam [] ["x"] $ Con "Just" []) $
  Let "i" [] (Lit $ LiteralI64 10) $
  Var "fn" ["i", "i"]

test9 :: GCode
test9 =
  Let "i" [] (Lit $ LiteralI64 10) $
  Let "fn" ["i"] (Con "Just" ["i"]) $
  Let "i" [] (Lit $ LiteralI64 12) $
  Var "fn" ["i"]

test10 :: GCode
test10 =
  Let "i" [] (Lit $ LiteralI64 0) $
  Case "i" Nothing
    []

test11 :: GCode
test11 =
  Let "i" [] (Lit $ LiteralI64 1) $
  Case "i" (Just $ Con "NonZero" [])
    [Alt (LitPattern $ LiteralI64 0) $ Con "Zero" []]

test12 :: GCode
test12 = -- isNothing
  Let "v" [] (Con "Nothing" []) $
  Case "v" (Just $ Con "False" [])
    [Alt (ConPattern "Nothing" []) $ Con "True" []]

test13 :: GCode
test13 = -- fromJust
  Let "i" [] (Con "InsideJust" []) $
  Let "v" ["i"] (Con "Just" ["i"]) $
  Case "v" Nothing $
    [Alt (ConPattern "Just" ["a"]) $ Var "a" []]

test14 :: GCode
test14 =
  Let "i" [] (Lit $ LiteralI64 10) $
  Let "_" ["i"] (Var "printLit#" ["i"]) $
  Case "_" (Just $ Con "Done" []) []

test15 :: GCode
test15 =
  Let "i" [] (Lit $ LiteralI64 10) $
  LetStrict "" (Var "printLit#" ["i"]) $
  Con "Done" []

test16 :: GCode
test16 =
  LetStrict "i" (Lit $ LiteralI64 1) $
  LetRec [("ones", ["ones", "i"], Con "Cons" ["i", "ones"])] $
  Var "ones" []

test17 :: GCode
test17 =
  LetRec [("loop", ["loop"], Var "loop" [])] $
  Case "loop" (Just $ Con "Done" []) []

test18 :: GCode
test18 =
  LetStrict "e" (Con "MyException" []) $
  Throw "e"

test19 :: GCode
test19 =
  Catch [] "exh" (Con "Caught" ["exh"]) $
  LetStrict "e" (Con "MyException" []) $
  Throw "e"
