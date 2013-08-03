-- Run-time compilation of GRIN.
-- Note: This is only a more efficient way of interpreting GRIN.
module Grin.Eval.Compile
    ( runGrin
    ) where

import Grin.Types hiding (Value(..))
import qualified Grin.Types as Grin
import Grin.Eval.Types
import Grin.Eval.Primitives
import Grin.Eval.Methods

import qualified Data.Map as Map
import Control.Monad.Reader

import Grin.Types hiding (Value(..))

import qualified Data.Map as Map
import Control.Monad.State
import Data.Char



runGrin :: Grin -> [String] -> IO EvalValue
runGrin grin commandArgs
    = let globalScope = GlobalScope { globalCAFs  = Map.fromList cafs
                                    , globalFuncs = Map.fromList (funcs ++ prims) }
          cafs = zip (map cafName (grinCAFs grin)) [0..]
          funcs = [ (funcDefName def, compFuncDef def globalScope) | def <- grinFunctions grin ]
          prims = listPrimitives globalScope
      in runComp $ do mapM_ storeValue =<< mapM (\caf -> compValue (cafValue caf) globalScope) (grinCAFs grin)
                      setCommandArgs ("lhc":commandArgs)
                      lookupFunction (grinEntryPoint grin) globalScope []

runComp comp
    = runReaderT (evalStateT (unComp comp) initState) Map.empty
    where initState = EvalState { stateHeap = Map.empty
                                , stateFree = 0
                                , stateArgs = ["lhc"] }

compFuncDef :: FuncDef -> Gen CompFunction
compFuncDef func
    = do exp <- compExpression (funcDefBody func)
         return $ \args -> local (const $ Map.fromList (zip (funcDefArgs func) args)) exp

compExpression :: Expression -> Gen CompExpression
compExpression (e :>> l)
    = do e' <- compExpression e
         l' <- compExpression l
         return $ e' >> l'
compExpression (e :>>= v :-> l)
    = do e' <- compExpression e
         l' <- compExpression l
         return $ do val <- e'
                     local (Map.insert v val) l'
compExpression (Application (External name) args)
    = runExternal name =<< mapM lookupVariable args
compExpression (Application name args)
    = do fn <- lookupFunction name
         args' <- mapM lookupVariable args
         return $ do args'' <- mapM id args'
                     fn args''
compExpression (Unit value)
    = compValue value
compExpression (Store val)
    = do val' <- compValue val
         return $ do ptr <- storeValue =<< val'
                     return $ HeapPointer ptr
compExpression (Case val alts)
    = do val' <- lookupVariable val
         let (binds,cases) = unzip [ (b,c) | b :> c <- alts ]
         cases' <- mapM compExpression cases
         return $ do val'' <- val'
                     --liftIO $ putStrLn $ "case expression: " ++ show (val,val'')
                     runCase val'' (zip binds cases')

compValue :: Grin.Value -> Gen CompValue
compValue (Grin.Node name Grin.ConstructorNode n args)
    = do args' <- mapM lookupVariable args
         return $ do args'' <- mapM id args'
                     return $ CNode name n args''
compValue (Grin.Node name Grin.FunctionNode n args)
    = do fn <- lookupFunction name
         args' <- mapM lookupVariable args
         return $ do args'' <- mapM id args'
                     return $ FNode name fn n args''
compValue (Grin.Variable v) = lookupVariable v
compValue Grin.Empty        = return $ return Empty
compValue (Grin.Lit lit)    = return $ return $ Lit lit
compValue (Grin.Hole size)  = return $ return $ Hole size
compValue (Grin.Vector vs)
    = do vs' <- mapM lookupVariable vs
         return $ do vs'' <- mapM id vs'
                     return $ Vector vs''

runCase :: EvalValue -> [(Grin.Value, CompValue)] -> CompValue
runCase val cases
    = worker cases
    where worker [(Grin.Variable name, e)]
              = local (Map.insert name val) e
          worker ((Grin.Variable name, e):_)
              = error "Default matches would always be last."
          worker ((b,c):xs)
              = case doesMatch val b of
                  Nothing -> worker xs
                  Just fn -> fn c
          worker [] = error $ "runCase: " ++ show (val, map fst cases)

doesMatch :: EvalValue -> Grin.Value -> Maybe (CompValue -> CompValue)
doesMatch val (Grin.Variable var)
    = Just $ local (Map.insert var val)
doesMatch (CNode tag _ args) (Grin.Node bTag _ _ bArgs) | tag == bTag && length args == length bArgs
    = bindLambdas (zip args $ map Grin.Variable bArgs)
doesMatch (Vector vs1) (Grin.Vector vs2) | length vs1 == length vs2
    = bindLambdas (zip vs1 $ map Grin.Variable vs2)
doesMatch (Lit litA) (Grin.Lit litB) | litA == litB
    = Just id
doesMatch (FNode name fn arity args) (Grin.Node tag FunctionNode n args') | name == tag && arity == n
    = bindLambdas (zip args $ map Grin.Variable args')
doesMatch _ Grin.Empty
    = Just id
doesMatch from to
    = Nothing


bindLambdas :: [(EvalValue, Grin.Value)] -> Maybe (CompValue -> CompValue)
bindLambdas [] = Just id
bindLambdas ((a,b):xs)
    = case (doesMatch a b, bindLambdas xs) of
        (Just fn, Just fn') -> Just (fn . fn')
        _                   -> Nothing




