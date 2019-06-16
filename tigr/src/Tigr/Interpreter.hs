{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tigr.Interpreter where

import Tigr.Types
import Tigr.Monad
import Tigr.Pretty

import Text.PrettyPrint.ANSI.Leijen (indent)
import           Control.Exception    (Exception (..), SomeException (..),
                                       handle, throwIO)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.IORef
import Data.Char
import           Data.Map             (Map)
import qualified Data.Map             as Map
import qualified Data.Set as Set
import Data.Set (Set)
import           Data.Typeable




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

evaluate :: GCode -> M Thunk
evaluate gcode = do
  -- liftIO $ putStrLn $ "Eval step: " ++ show (ppGCode gcode)
  case gcode of
    Var "expensive#" [] -> pure $ ThunkLit $ LiteralI64 0
    Var "_cast" [arg] -> lookupWhnf arg
    External "indexI8#" [arg] -> do
      ThunkLit (LiteralString str) <- lookupWhnf arg
      case str of
        []     -> pure $ ThunkLit $ LiteralI64 0
        (c:cs) -> pure $ ThunkLit $ LiteralI64 (ord c)
    External "addrAdd#" [ptr, offset] -> do
      ThunkLit (LiteralString str) <- lookupWhnf ptr
      ThunkLit (LiteralI64 n) <- lookupWhnf offset
      pure $ ThunkLit $ LiteralString (drop n str)
    External "putchar" [arg] -> do
      ThunkLit (LiteralI64 c) <- lookupWhnf arg
      liftIO $ putChar (chr c)
      pure $ ThunkLit $ LiteralI64 0
    External "getchar" [] -> do
      c <- liftIO getChar
      pure $ ThunkLit $ LiteralI64 (ord c)
    External "sdiv#" [arg0, arg1] -> do
      ThunkLit (LiteralI64 a) <- lookupWhnf arg0
      ThunkLit (LiteralI64 b) <- lookupWhnf arg1
      pure $ ThunkLit $ LiteralI64 $ a `div` b
    External "srem#" [arg0, arg1] -> do
      ThunkLit (LiteralI64 a) <- lookupWhnf arg0
      ThunkLit (LiteralI64 b) <- lookupWhnf arg1
      pure $ ThunkLit $ LiteralI64 $ a `rem` b
    External "+#" [arg0, arg1] -> do
      ThunkLit (LiteralI64 a) <- lookupWhnf arg0
      ThunkLit (LiteralI64 b) <- lookupWhnf arg1
      pure $ ThunkLit $ LiteralI64 $ a + b
    External "-#" [arg0, arg1] -> do
      ThunkLit (LiteralI64 a) <- lookupWhnf arg0
      ThunkLit (LiteralI64 b) <- lookupWhnf arg1
      pure $ ThunkLit $ LiteralI64 $ a - b
    External "*#" [arg0, arg1] -> do
      ThunkLit (LiteralI64 a) <- lookupWhnf arg0
      ThunkLit (LiteralI64 b) <- lookupWhnf arg1
      pure $ ThunkLit $ LiteralI64 $ a * b
    Var "printLit#" [thunk] -> do
      ThunkLit lit <- lookupWhnf thunk
      liftIO $ print lit
      pure $ ThunkLit $ LiteralI64 0
    Var val [] -> lookupWhnf val
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
    _ -> error $ "Unhandled gcode: " ++ show gcode

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
  putStrLn (show (indent i (ppGCode gcode)))

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

finalize :: Context -> IO ()
finalize ctx =
    forM_ (Map.elems ctx) $ \ref ->
      modifyIORef ref finalizeThunk
  where
    finalizeThunk :: Thunk -> Thunk
    finalizeThunk (ThunkClosure (Closure env gcode)) = ThunkClosure (Closure env $ finalizeGCode gcode)
    finalizeThunk t = t
    finalizeGCode :: GCode -> GCode
    finalizeGCode gcode =
      case gcode of
        Var{} -> gcode
        Con{} -> gcode
        External{} -> gcode
        Let bind _free rhs body ->
          Let bind (Set.toList $ free rhs)
            (finalizeGCode rhs)
            (finalizeGCode body)
        LetRec binds body ->
          let bound = Set.fromList [ name | (name, _, _) <- binds ]
          in LetRec
              [ (bind, Set.toList $ free rhs, finalizeGCode rhs)
              | (bind, _free, rhs) <- binds ]
              (finalizeGCode body)
        LetStrict bind rhs body ->
          LetStrict bind (finalizeGCode rhs) (finalizeGCode body)
        Lit{} -> gcode
        Case scrut mbDef alts ->
          Case scrut (fmap finalizeGCode mbDef)
            [ Alt pattern (finalizeGCode branch) | Alt pattern branch <- alts ]
        Lam _free bound body ->
          Lam (Set.toList $ free gcode)
            bound (finalizeGCode body)
        Throw{} -> gcode
        Catch _free bound handler body ->
          Catch (Set.toList $ Set.delete bound $ free handler)
            bound (finalizeGCode handler) (finalizeGCode body)
    free gcode = Set.delete "_cast" (freeVars gcode) `Set.difference` Map.keysSet ctx
    freeVars :: GCode -> Set Name
    freeVars (Var v args) = Set.fromList (v:args)
    freeVars (Con _ args) = Set.fromList (args)
    freeVars (External _cName args) = Set.fromList args
    freeVars (Let name _free rhs body) = Set.delete name (freeVars body) `Set.union` freeVars rhs
    freeVars (LetRec binds body) =
      let bound = [ name | (name, _, _) <- binds ]
          rhss = [ rhs | (_, _, rhs) <- binds ]
      in Set.unions (freeVars body : map freeVars rhss) `Set.difference` Set.fromList bound
    freeVars (LetStrict name rhs body) =
      Set.delete name (freeVars body) `Set.union` freeVars rhs
    freeVars Lit{} = Set.empty
    freeVars (Case scrut mbDef alts) =
      Set.insert scrut (maybe Set.empty freeVars mbDef) `Set.union`
      Set.unions (map freeVarsAlt alts)
    freeVars (Lam _free bound body) = freeVars body `Set.difference` Set.fromList bound
    freeVars (Throw name) = Set.singleton name
    freeVars (Catch _free bound handler body) =
      Set.delete bound (freeVars handler) `Set.union` freeVars body

    freeVarsAlt (Alt LitPattern{} branch) = freeVars branch
    freeVarsAlt (Alt (ConPattern _con args) branch) = freeVars branch `Set.difference` Set.fromList args
{-
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
-}
runTest :: GCode -> IO ()
runTest x = printThunk 0 =<< run x

runThunk :: Context -> Name -> IO ()
runThunk ctx name = printThunk 0 =<< unM (lookupWhnf name) ctx Map.empty

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
