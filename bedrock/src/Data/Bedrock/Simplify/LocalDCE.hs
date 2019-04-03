-- Local Dead Code Elimination
module Data.Bedrock.Simplify.LocalDCE
    ( localDCE ) where

import           Data.Bedrock
import           Data.Bedrock.Transform

import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Data.Map     (Map)
import qualified Data.Map     as Map
import Control.Monad.Reader
import Control.Monad.Writer

localDCE :: Module -> Module
localDCE m =
  m{ functions = map dceFunction (functions m) }


newtype Out = Out (Map Variable (Set Variable))
type In = Set Variable
type M a = ReaderT In (Writer Out) a

instance Semigroup Out where
  Out m1 <> Out m2 = Out (Map.unionWith Set.union m1 m2)

instance Monoid Out where
  mempty = Out Map.empty

dceFunction :: Function -> Function
dceFunction fn =
    fn{ fnBody = body }
  where
    root = Variable (fnName fn) Node
    (body, out) = runWriter (runReaderT (dceBlock root (fnBody fn)) dirty)
    dirty = findUsed root out

findUsed :: Variable -> Out -> Set Variable
findUsed root (Out m0) = worker [root] m0
  where
    worker [] _     = Set.empty
    worker (x:xs) m =
      case Map.lookup x m of
        Nothing   -> worker xs m
        Just deps -> Set.union deps $ worker (Set.toList deps ++ xs) (Map.delete x m)

useMany :: [Variable] -> [Variable] -> M ()
useMany keys vars =
    forM_ keys $ \key -> tell (Out $ Map.singleton key set)
  where
    set = Set.fromList vars

onMaybe :: (a -> M b) -> Maybe a -> M (Maybe b)
onMaybe _ Nothing   = return Nothing
onMaybe fn (Just a) = fn a >>= return . Just

ifUsed :: [Variable] -> (Block -> Block) -> M (Block -> Block)
ifUsed lst fn = asks $ \dirty ->
  if any (`Set.member` dirty) lst
    then fn
    else id

dceBlock :: Variable -> Block -> M Block
dceBlock root block =
  case block of
    Case scrut mbDefault alts -> do
      useMany [root] [scrut]
      Case scrut
        <$> onMaybe (dceBlock root) mbDefault
        <*> sequence
          [ Alternative pattern <$> dceBlock root branch
          | Alternative pattern branch <- alts ]
    Bind [] expr rest -> do
      let deps = freeVariablesSimple expr Set.empty
      useMany [root] (Set.toList deps)
      Bind [] expr <$> dceBlock root rest
    Bind vars expr rest -> do
      let deps = freeVariablesSimple expr Set.empty
      useMany vars (Set.toList deps)
      unless (isSimple expr) $
        useMany [root] vars
      (ifUsed vars $ Bind vars expr) <*> dceBlock root rest
    Recursive binds rest ->
      Recursive binds <$> dceBlock root rest
    Return vars -> do
      useMany [root] vars
      return block
    Raise var -> do
      useMany [root] [var]
      return block
    TailCall _fn args -> do
      useMany [root] args
      return block
    Invoke a args -> do
      useMany [root] (a:args)
      return block
    Exit    -> return block
    Panic{} -> return block

isSimple :: Expression -> Bool
isSimple Store{} = True
isSimple StoreAlloc{} = True
isSimple StoreWrite{} = True
isSimple MkNode{} = True
isSimple Literal{} = True
isSimple TypeCast{} = True
isSimple Undefined{} = True
isSimple Address{} = True
isSimple Fetch{} = True
isSimple Load{} = True
isSimple _ = False
