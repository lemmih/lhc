module Data.Bedrock.InlineByCost ( inline ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.State
import           Data.Bedrock
import           Data.Map (Map)
import qualified Data.Map            as Map

data Cost
  = ProhibitlyExpensive
  | Cost Int [Variable] ([Variable] -> Block -> Block)

type Env = Map Name (Either Function Cost)
type M a = State Env a

setFunctionCost :: Name -> Cost -> M ()
setFunctionCost name cost =
  modify $ Map.insert name (Right cost)

getFunctionCost :: Name -> M Cost
getFunctionCost name = do
  mbEntry <- gets (Map.lookup name)
  case mbEntry of
    Nothing -> return ProhibitlyExpensive
    Just (Right cost) -> return cost
    Just (Left fn) -> do
      setFunctionCost name ProhibitlyExpensive
      mbCont <- blockCost (fnBody fn)
      case mbCont of
        Nothing -> return ProhibitlyExpensive
        Just (n, inliner) -> do
          let cost = Cost n (fnArguments fn) inliner
          setFunctionCost name cost
          return cost

blockCost :: Block -> M (Maybe (Int, [Variable] -> Block -> Block))
blockCost block =
  case block of
    Case scrut defaultBranch alts -> return Nothing
    Bind vars expr rest ->
      fmap (step (exprCost expr) (\_ -> Bind vars expr)) <$> (blockCost rest)
    Return rets -> return $ Just (0, \vars -> typeCastMany (zip vars rets))
    Raise{} -> return $ Just (0, \_ -> const block)
    TailCall{} -> return Nothing
    Invoke{} -> return Nothing
    InvokeHandler{} -> return Nothing
    Exit -> return Nothing
    Panic{} -> return $ Just (0, \_ -> const block)
  where
    step n fn (n', fn') = (n+n', \vars -> fn vars . fn' vars)
    exprCost expr =
      case expr of
        Eval{} -> 0
        Apply{} -> 0
        Literal{} -> 0
        CCall fn _ -> ccallCost fn
        _ -> 1
    ccallCost _ = 0

typeCastMany :: [(Variable, Variable)] -> Block -> Block
typeCastMany [] = id
typeCastMany ((dst, src):xs) = Bind [dst] (TypeCast src) . typeCastMany xs






inline :: Module -> Module
inline m = evalState (inlineModule m) env
  where
    env = Map.fromList [ (fnName fn, Left fn) | fn <- functions m ]

inlineModule :: Module -> M Module
inlineModule m = do
  fns <- mapM inlineFunction (functions m)
  return m{ functions = fns }

inlineFunction :: Function -> M Function
inlineFunction fn = do
  body <- inlineBlock (fnBody fn)
  return fn{fnBody=body}

inlineBlock :: Block -> M Block
inlineBlock block =
  case block of
    Case scrut defaultBranch alts ->
      Case scrut
        <$> maybe (return Nothing) (fmap Just . inlineBlock) defaultBranch
        <*> mapM inlineAlternative alts
    Bind vars expr@(Application fn args) rest -> do
      cost <- getFunctionCost fn
      case cost of
        Cost n inlinedArgs inliner | n <= 0 ->
          (typeCastMany (zip inlinedArgs args) .
          inliner vars) <$> inlineBlock rest
        _ProhibitlyExpensive -> Bind vars expr <$> inlineBlock rest
    Bind vars expr rest ->
      Bind vars expr <$> inlineBlock rest
    Return{} -> return block
    Raise{} -> return block
    TailCall{} -> return block -- FIXME
    Invoke{} -> return block
    InvokeHandler{} -> return block
    Exit -> return block
    Panic{} -> return block

inlineAlternative :: Alternative -> M Alternative
inlineAlternative (Alternative pattern branch) =
  Alternative pattern <$> inlineBlock branch
