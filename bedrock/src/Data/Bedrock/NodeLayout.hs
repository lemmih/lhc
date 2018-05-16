module Data.Bedrock.NodeLayout (finalizeNodeLayout) where

import           Data.Set     (Set)
import qualified Data.Set     as Set
import Control.Monad.Writer
import Data.List

import           Data.Bedrock

-- Scan through bedrock code and find all used nodes (data nodes and function
-- nodes).
-- Rearrange layout such that primitives are first and heap pointers are last.
-- Add all nodes layouts to the module.


finalizeNodeLayout :: Module -> Module
finalizeNodeLayout m = m
    { nodes = []
    , modLayouts = Set.toList layouts
    , functions = fs }
  where
    (fs, layouts) = runWriter (mapM finFunction (functions m))


type M a = Writer (Set NodeLayout) a
type Fin a = a -> M a

finFunction :: Fin Function
finFunction fn = do
  body <- finBlock (fnBody fn)
  return fn{fnBody = body}

finBlock :: Fin Block
finBlock block =
  case block of
    Case scrut mbBlock alts ->
      Case scrut
        <$> finMaybe finBlock mbBlock
        <*> mapM finAlternative alts
    Bind binds expr rest ->
      Bind binds
        <$> finExpression expr
        <*> finBlock rest
    Return{} -> pure block
    Raise{} -> pure block
    TailCall{} -> pure block
    Invoke{} -> pure block
    Exit -> pure block
    Panic{} -> pure block

finMaybe :: Fin a -> Fin (Maybe a)
finMaybe _ Nothing   = pure Nothing
finMaybe fn (Just a) = Just <$> fn a

finAlternative :: Fin Alternative
finAlternative (Alternative p rest) =
  Alternative <$> finPattern p <*> finBlock rest

finPattern :: Fin Pattern
finPattern pat =
  case pat of
    NodePat name vars -> NodePat name <$> touch name vars
    LitPat{}          -> pure pat

finExpression :: Fin Expression
finExpression expr =
  case expr of
    Application{}   -> pure expr
    CCall{}         -> pure expr
    Catch{}         -> pure expr
    InvokeReturn{}  -> pure expr
    Literal{}       -> pure expr
    Store node args -> Store node <$> touch node args
    Builtin{}       -> pure expr

touch :: NodeName -> [Variable] -> M [Variable]
touch name vars = do
    tell $ Set.singleton layout
    return $ primitives ++ heapPointers
  where
    layout = NodeLayout
      { layoutName = name
      , layoutPrimitives = length primitives
      , layoutPointers = length heapPointers }
    (primitives, heapPointers) = partition (isPrimitive . variableType) vars

isPrimitive :: Type -> Bool
isPrimitive NodePtr      = False
isPrimitive Node         = False
isPrimitive StaticNode{} = False
isPrimitive IWord        = True
isPrimitive Primitive{}  = True
isPrimitive FramePtr     = False
