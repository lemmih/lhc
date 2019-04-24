{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp           #-}
{-# LANGUAGE RecordWildCards            #-}
module Interpret (interpret) where

import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char
import           Data.Int
import           Data.IORef
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe
import qualified Data.Set                            as Set
import qualified Data.Vector                         as V
import qualified Data.Vector.Mutable                 as V (new, read, write)
import           Data.Word
import           Foreign.LibFFI
import           Foreign.Ptr
import           Language.Haskell.Crux
import           Language.Haskell.Crux.FreeVariables
import           Language.Haskell.Scope              (QualifiedName (..))
import           Language.Haskell.TypeCheck
import           Language.Haskell.TypeCheck.Pretty   hiding ((<$>))
import           System.IO
import           System.IO.Unsafe
import           System.Posix.DynamicLinker
import qualified Text.PrettyPrint.ANSI.Leijen        as Doc

interpret :: Module -> IO ()
interpret m =
    case mbStart of
      Nothing -> error "entrypoint not found"
      Just body -> do
        hSetBuffering stdin NoBuffering
        insts <- runGen $ do
          forM_ defs $ \(var, _) ->
            addTopLevel var
          topLevels <- gets genTopLevel
          forM_ defs $ \(var, def) -> do
            insts <- listenInstructions $ compileExpr def
            liftIO $ putStrLn $ show $ pretty var <$$> indent 2 (vsep $ map pretty insts)
            -- liftIO $ putStrLn "toplevel"
            liftIO $ writeIORef (topLevels Map.! var) insts
          compileExpr body
        displayIO stdout (renderPretty 1 100 $ vsep $ map pretty insts)
        putStrLn ""
        stack <- runProgram insts
        V.mapM_ (print.pretty) stack
  where
    defs =
      [ (Variable name ty, body)
      | Declaration ty name body <- cruxDecls m ]
    mbStart = listToMaybe
      [ body
      | Declaration ty name body <- cruxDecls m, name == entrypoint ]

entrypoint = Name ["Main"] "entrypoint" 0

type HP a = IORef a

-- newtype Scope = Scope (Map Variable (HP Expr))

-- emptyScope :: Scope
-- emptyScope = Scope Map.empty
--
-- lookupScope :: Variable -> Scope -> Maybe (HP Expr)
-- lookupScope key (Scope m) = Map.lookup key m
--
-- insertScope :: Variable -> HP Expr -> Scope -> Scope
-- insertScope key val (Scope m) = Scope (Map.insert key val m)
--
-- data Env = Env
--   { envScope    :: Scope
--   , envBindings :: Scope }
--
-- newtype IM a = IM (ReaderT Env IO a)
--   deriving (MonadIO, MonadReader Env, Monad, Applicative, Functor)

-- runIM :: IM a -> IO a
-- runIM (IM action) = runReaderT action env
--   where
--     env = Env emptyScope emptyScope

-- interleaveIM :: IM a -> IM a
-- interleaveIM (IM action) = IM $ ReaderT $ \env ->
--   unsafeInterleaveIO (runReaderT action env)

-- Variable -> (Scope, HP Expr)

-- let x = y z
-- in (x,x)

-- bind :: Variable -> Expr -> IM a -> IM a
-- bind v e action = do
--   -- liftIO $ putStrLn $ "Bind: " ++ show v
--   ref <- liftIO $ newIORef e
--   local (\env -> env{envScope = insertScope v ref (envScope env)}) action

-- bindMany :: [(Variable, Expr)] -> IM a -> IM a
-- bindMany []             = id
-- bindMany ((key,val):xs) = bind key val . bindMany xs
--
-- define :: [(Variable, Expr)] -> IM a -> IM a
-- define defs action = do
--   let (vs, es) = unzip defs
--   refs <- liftIO $ mapM newIORef es
--   local (\env -> env{envBindings = Scope $ Map.fromList (zip vs refs)}) action
--
-- requireHP :: Variable -> IM (HP Expr)
-- requireHP var = do
--   Env{..} <- ask
--   case listToMaybe (mapMaybe (lookupScope var) [envScope, envBindings]) of
--     Just val -> pure val
--     Nothing  -> error $ "var not found: " ++ show var
--
-- withScope :: Scope -> IM a -> IM a
-- withScope scope = local (\env -> env{envScope = scope})
--
-- deepSeq :: Expr -> IM Expr
-- deepSeq e = do
--   e' <- eval e
--   case e' of
--     App a b -> do
--       App a <$> deepSeq b
--     _ -> pure e'

{-
PushCode [PushLit "name", Enter unpackString]
PushRef putStrLn
Enter (0 -> hp, 1 -> s)
-- (1 -> s', 0 -> ())
Dead 0
PushRef getLine
Enter
-- (1 -> s', 0 -> name)
Hoist 1 -- (1 -> name)
PushCode [PushLit "Hi ", Enter unpackString]
PushRef putStr
Enter
-- (2 -> name, 1 -> s'', 0 -> ())
Dead 0 -- (1 -> name, 0 -> s'')
Hoist 1 -- (1 -> s'', 0 -> name)
PushRef putStr
Enter
-- (1 -> s''', 0 -> ())
Dead 0 -- (0 -> s''')
PushCode [PushLit ".", Enter unpackString])
PushRef putStrLn
Enter
-}

{-
\b a ->
  case b of
    True -> False
    False -> True

Dead 0
Enter
SwitchCon True [PushCon False]
SwitchCon False [PushCon True]
-}
{-
\a b c -> Just (fn b a)

Copy 2
Copy 2
Copy 2
PushCode 3 [Hoist 2, Hoist 2, PushRef fn, Enter]
PushCon Just

Dead
PushCode 2 [Hoist 1, PushRef fn, Enter]
PushCon Just

\a b -> Just (a b)
\a b 2 1
PushClosure [0,1] [Hoist 1, Enter]
Hoist 1
Dead
Hoist 1
Dead
Enter
-}
data Element
  = ElementClosure [Element] (HP [Instruction])
  | ElementLit Literal
  | ElementCon Variable

data Instruction
  = PushClosure [Int] [Instruction]
  | PushLit !Literal
  | PushCon !Variable
  | PushRef !Reference
  | Enter
  -- | Hoist Int -- Same as Copy nth >> Dead nth but faster.
  | Dead !Int
  | Copy !Int
  | SwitchCon Variable [Instruction]
  | SwitchLit Literal [Instruction]
  | RunExternal String [Type] Type
  deriving (Show, Eq)

data Reference = Reference Variable (HP [Instruction])
  deriving (Eq)

instance Show Reference where
  show (Reference v _) = show v

newtype Gen a = Gen (StateT GenST IO a)
  deriving (Monad, MonadState GenST, MonadIO, Applicative, Functor)

data GenST = GenST
  { genInstructions :: [Instruction]
  , genStack        :: Map Variable Int
  , genTopLevel     :: Map Variable (HP [Instruction])
  }

runGen :: Gen () -> IO [Instruction]
runGen (Gen action) =
    optimizeFixpoint.reverse.genInstructions
      <$> execStateT action st
  where
    st = GenST [] Map.empty Map.empty

addTopLevel :: Variable -> Gen ()
addTopLevel v = do
  hp <- liftIO $ newIORef []
  modify $ \st -> st{genTopLevel = Map.insert v hp (genTopLevel st)}

modifyStack :: (Map Variable Int -> Map Variable Int) -> Gen ()
modifyStack fn = modify $ \st -> st{ genStack = fn (genStack st) }

listenInstructions :: Gen () -> Gen [Instruction]
listenInstructions gen = do
  st <- get
  put st{genInstructions = []}
  gen
  insts <- gets genInstructions
  put st
  return $ optimizeFixpoint $ reverse insts

pushInst :: Instruction -> Gen ()
pushInst !inst = do
    -- liftIO $ putStrLn $ "Push " ++ show (pretty inst)
    modify $ \st -> st{ genInstructions = inst : genInstructions st }
    when (incStack inst) $
      modifyStack $ Map.map (+1)
    when (inst == Enter) $
      modifyStack $ Map.map (subtract 1)
  where
    incStack PushClosure{} = True
    incStack PushLit{}     = True
    incStack PushCon{}     = True
    incStack PushRef{}     = True
    incStack Copy{}        = True
    incStack _             = False


kill :: Variable -> Gen ()
kill var = do
  stack <- gets genStack
  -- liftIO $ putStrLn $ "kill: " ++ show (pretty var)
  let nth = stack Map.! var
  pushInst (Dead nth)
  modifyStack $
    Map.map (\idx -> if idx > nth then idx-1 else idx) .
    Map.delete var

markDead :: Expr -> Gen ()
markDead e = do
    stack <- gets genStack
    let dead = Map.keysSet stack `Set.difference` free
    forM_ (Set.toList dead) kill
  where
    free = freeVariablesExpr_ e

killEverything :: Gen ()
killEverything = do
  stack <- gets genStack
  case Map.minViewWithKey stack of
    Nothing -> return ()
    Just ((var,_idx), _) -> do
      kill var
      killEverything

assumePushed :: Variable -> Gen ()
assumePushed v =
  modifyStack $ Map.insert v 0 . Map.map (+1)
{-
\a ->
let rec repeat = a : repeat
in head repeat

repeat_a a = a : repeat_a

\a ->
let repeat = repeat_a a
in head repeat
PushClosure [0] [PushRef repeat_a, Enter]
Dead 0
PushRef head
Enter


let rec fromOne = 1 : fromZero
        fromZero = 0 : fromOne

-}
compileClosure :: Expr -> Gen ()
compileClosure (Con con) = pushInst (PushCon con)
compileClosure (Lit lit) = pushInst (PushLit lit)
-- compileClosure (Var v) = do
--   st <- gets genStack
--   case Map.lookup v st of
--     Nothing -> error "global"
--     Just n -> pushInst (Copy n)
compileClosure e = do
    topLevel <- gets (Map.keysSet . genTopLevel)
    stack <- gets genStack
    let free' = free `Set.difference` topLevel
    let bad = free' `Set.difference` Map.keysSet stack
    -- liftIO $ putStrLn $ "closure: " ++ show bad
    let lst = map (stack Map.! ) (Set.toList free')
    insts <- listenInstructions $ do
      modifyStack (const $ Map.fromList $ zip (Set.toList free') [0..])
      compileExpr e
    pushInst (PushClosure lst insts)
  where
    free = freeVariablesExpr_ e

compileStrict :: Expr -> Gen ()
compileStrict e = do
  compileClosure e
  pushInst Enter

compileExpr :: Expr -> Gen ()
compileExpr e = do
    markDead e
    -- liftIO $ putStrLn $ "Compiling:"
    -- liftIO $ displayIO stdout (renderCompact $ pretty e)
    case e of
      Var v -> do
        st <- gets genStack
        case Map.lookup v st of
          Nothing -> do
            topLevel <- gets genTopLevel
            case Map.lookup v topLevel of
              Nothing -> error $ "Identifier not found: " ++ show (pretty v)
              Just hp -> do
                pushInst (PushRef (Reference v hp))
                pushInst Enter
          Just 0 -> pushInst Enter
          Just n -> do
            pushInst (Copy n)
            kill v
            pushInst Enter
      -- if local => Copy idx >> Enter
      -- if global => PushRef >> Enter
      Con con -> pushInst (PushCon con)
      Lit lit -> pushInst (PushLit lit)
      App fn a -> do
        compileClosure a
        compileExpr fn
      Lam vs body -> do
        mapM_ assumePushed (reverse vs)
        compileExpr body
      Case scrut binder mbDef alts -> do
        compileStrict scrut
        forM_ alts $ \(Alt pat altBody) -> do
          case pat of
            ConPat con binds -> do
              altInsts <- listenInstructions $ do
                assumePushed binder
                mapM_ assumePushed (reverse binds)
                compileExpr altBody
              pushInst $ SwitchCon con altInsts
            LitPat lit -> do
              altInsts <- listenInstructions $ do
                assumePushed binder
                compileExpr altBody
              pushInst $ SwitchLit lit altInsts
            UnboxedPat binds -> do
              mapM_ assumePushed (reverse binds)
              compileExpr altBody
        case mbDef of
          Nothing -> return ()
          Just def -> do
            assumePushed binder
            compileExpr def
      ExternalPure out fn args st -> do
        mapM_ compileStrict (reverse args)
        killEverything
        pushInst $ RunExternal fn (map exprType args) (varType out)
      WithExternal outV retS fn args st rest -> do
        mapM_ compileStrict (reverse args)
        pushInst $ RunExternal fn (map exprType args) (varType outV)
        assumePushed outV
        pushInst (PushLit LitVoid)
        assumePushed retS
        compileExpr rest
      Convert e' _ -> compileExpr e'
      Cast -> return ()
      -- LetStrict binder body rest
      Let (NonRec bind body) rest -> do
        compileClosure body
        assumePushed bind
        compileExpr rest
      UnboxedTuple exprs -> do
        mapM_ compileClosure (reverse exprs)
      _ -> error $ "Unhandled expr: " ++ show (pretty e)
--       -- assume arguments have been pushed to the stack
--     Let (NonRec key val) body -> do
--       -- push closure
--     Let (Rec binds) body -> do
--
--     Var Variable -> done
--     Con Variable -> done
--     UnboxedTuple [Expr] -> done
--     Lit Literal -> done
--     WithExternal Variable Variable String [Expr] Expr Expr
--     ExternalPure Variable String [Expr] Expr
--     App Expr Expr -> done
--     Lam [Variable] Expr -> done
--     Let LetBind Expr -> done
--     LetStrict Variable Expr Expr -> done
--     Case Expr Variable (Maybe Expr) [Alt]
--     Convert Expr Type
--     Cast

exprType :: Expr -> Type
exprType e =
  case e of
    Var v -> varType v
    Lit LitI8{} -> error "I8"
    Lit LitI16{} -> error "I16"
    Lit LitI32{} -> error "I32"
    Lit LitI64{} -> TyCon (QualifiedName "LHC.Prim" "I64")
    _ -> error $ "exprType: " ++ show (pretty e)

optimizeFixpoint :: [Instruction] -> [Instruction]
optimizeFixpoint orig
    | orig == opt = opt
    | otherwise   = optimizeFixpoint opt
  where
    opt = reverse . optimizeReverse . reverse . optimize $ orig

optimizeReverse :: [Instruction] -> [Instruction]
optimizeReverse []     = []
-- optimizeReverse (Dead:Dead:Hoist n:xs) = optimizeReverse (Dead:Hoist (n-1):Dead:xs)
-- optimizeReverse (Hoist 0:xs) = optimizeReverse xs
-- optimizeReverse (Dead a:Copy b:xs) | a > b+1 =
--   optimizeReverse $ Copy b : Dead (a-1) : xs
-- optimizeReverse (Dead 1:Copy 0:xs)             = optimizeReverse xs
-- optimizeReverse (Dead 0:Copy 1:xs)             = optimizeReverse xs
-- optimizeReverse (Dead a:Copy b:xs)
--   | a==b+1 = optimizeReverse xs
-- optimizeReverse (PushClosure [n] [Enter] : xs) = optimizeReverse (Copy n : xs)
optimizeReverse (x:xs) = x : optimizeReverse xs

optimize :: [Instruction] -> [Instruction]
optimize insts =
    case insts of
      []     -> []
      -- (Copy a : Dead b : xs) | a+1 == b ->
      --   optimize $ map (mapStack (subtract 1)) xs
      -- (Copy a : Dead b : Copy a' : Dead b' : xs) | a==a' && b==b' ->
      --   optimize $ xs
      (PushClosure [n] [Enter] : xs) ->
        optimize $ Copy n : xs
      -- (PushClosure dat insts:Enter:xs) ->
      --   optimize ([ Copy idx | idx <- reverse dat ] ++ insts ++ xs)
      -- (PushClosure [] insts:Enter:xs) -> optimize (insts++xs)
      (x:xs) -> x : optimize xs
  where
    mapStack fn inst =
      case inst of
        PushClosure dat insts -> PushClosure (map fn dat) insts
        PushLit{}             -> inst
        PushCon{}             -> inst
        PushRef{}             -> inst
        Enter                 -> inst
        Dead nth              -> Dead (fn nth)
        Copy nth              -> Copy (fn nth)
        SwitchCon con insts   -> SwitchCon con (map (mapStack fn) insts)
        SwitchLit lit insts   -> SwitchLit lit (map (mapStack fn) insts)


toFFIRet :: Type -> RetType Expr
toFFIRet (TyCon (QualifiedName "LHC.Prim" "I32")) =
  fmap (\w -> Lit $ LitI32 w) retWord32

toFFIArg :: Expr -> Arg
toFFIArg (Lit (LitI32 n))  = argWord32 n
toFFIArg (Lit (LitChar c)) = argWord32 (fromIntegral $ ord c)
toFFIArg e                 = error $ "toFFIArg: " ++ show e

blackhole (Variable n t) =
  Variable (Name [] "Blackhole" 0) t

instance Pretty Element where
  pretty (ElementClosure _ _) = text "Closure"
  pretty (ElementLit lit)     = text "Lit" <+> pretty lit
  pretty (ElementCon con)     = text "Con" <+> pretty con

instance Pretty Instruction where
    pretty inst =
      case inst of
        PushClosure dat insts ->
          text "Closure" <+> ppList (map Doc.int dat) <$$>
          indent 2 (vsep $ map pretty insts)
        PushLit lit ->
          text "Push" <+> pretty lit
        PushCon con ->
          text "Push" <+> pretty con
        PushRef (Reference v hp) ->
          text "Push" <+> pretty v
        Enter ->
          text "Enter"
        -- Hoist nth ->
        --   text "Hoist" <+> Doc.int nth
        Dead nth ->
          text "Dead" <+> Doc.int nth
        Copy nth ->
          text "Copy" <+> Doc.int nth
        SwitchCon con insts ->
          text "Switch" <+> pretty con <$$>
          indent 2 (vsep $ map pretty insts)
        SwitchLit lit insts ->
          text "Switch" <+> pretty lit <$$>
          indent 2 (vsep $ map pretty insts)
        RunExternal fn args ret ->
          text "External" <+> Doc.text fn <+>
          Doc.brackets (ppList (map pretty args)) <+> Doc.text "->" <+> pretty ret


type EvalStack = V.MVector RealWorld Element
newtype Eval a = Eval {unEval :: ReaderT EvalStack (StateT Int IO) a }
  deriving (Monad, MonadReader EvalStack, MonadState Int, Applicative, Functor, MonadIO)

runProgram :: [Instruction] -> IO (V.Vector Element)
runProgram insts = do
  stack <- V.new (1024*16)
  n <- execStateT (runReaderT (unEval $ mapM_ runInst insts) stack) 0
  V.take n <$> V.freeze stack

evalPush :: Element -> Eval ()
evalPush elt = do
  nth <- get
  put (nth+1)
  stack <- ask
  liftIO $ V.write stack nth elt

runInst :: Instruction -> Eval ()
runInst inst = do
  stack <- ask
  case inst of
    PushClosure dat insts -> do
      offset <- get
      copied <- liftIO $ sequence [ V.read stack (offset-1-idx) | idx <- dat ]
      hp <- liftIO $ newIORef insts
      evalPush (ElementClosure copied hp)
    PushLit lit -> evalPush (ElementLit lit)
    PushCon var -> evalPush (ElementCon var)
    PushRef (Reference _var hp) ->
      evalPush (ElementClosure [] hp)
    Enter -> do
      offset <- get
      elt <- liftIO $ V.read stack (offset-1)
      case elt of
        ElementClosure dat hp -> do
          put (offset-1)
          mapM_ evalPush (reverse dat)
          insts <- liftIO $ readIORef hp
          mapM_ runInst insts
        _ -> return () -- FIXME: Print warning. This should never happen if the
                       -- bytecode was correctly optimized.
    Dead nth -> do
      offset <- get
      forM_ [nth,nth-1..1] $ \idx -> do
        elt <- liftIO $ V.read stack (offset-1-nth+1)
        liftIO $ V.write stack (offset-1-nth) elt
      put (offset-1)
    Copy nth -> do
      offset <- get
      elt <- liftIO $ V.read stack (offset-1-nth)
      evalPush elt
    RunExternal "indexI8#" [_ptrTy] _retTy -> do
      modify (subtract 1)
      offset <- get
      elt <- liftIO $ V.read stack offset
      case elt of
        ElementLit (LitString (c:cs)) -> evalPush (ElementLit (LitI8 $ fromIntegral $ ord c))
    -- RunExternal fn argTys retTy -> do
    --   args <- forM argTys $ \argTy -> do
    --     case argTy of
    --       TyCon (QualifiedName "LHC.Prim" "I8")
    _           -> error $ "Unknown inst: " ++ show (pretty inst)

--data Reference = Reference Variable (HP [Instruction])
-- data Element
--   = ElementClosure [Element] (HP [Instruction])
--   | ElementLit Literal
--   | ElementCon Variable
--
-- data Instruction
--   = PushClosure [Int] [Instruction]
--   | PushLit !Literal
--   | PushCon !Variable
--   | PushRef !Reference
--   | Enter
--   -- | Hoist Int -- Same as Copy nth >> Dead nth but faster.
--   | Dead !Int
--   | Copy !Int
--   | SwitchCon Variable [Instruction]
--   | SwitchLit Literal [Instruction]
--   | RunExternal String [Type] Type
