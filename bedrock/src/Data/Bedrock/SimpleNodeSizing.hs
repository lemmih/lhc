{-# LANGUAGE PatternGuards #-}
module Data.Bedrock.SimpleNodeSizing (lowerNodeSize) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.Map               as Map

import           Data.Bedrock
import           Data.Bedrock.Transform

lowerNodeSize :: Gen ()
lowerNodeSize = do
  fs <- gets (Map.elems . envFunctions)
  forM_ fs $ \fn ->
    pushFunction fn{fnBody = lower (fnBody fn)}

lower :: Block -> Block
lower block =
  case block of
    Bind [obj1] (Fetch ptr) (Case obj2 mbDefault alts)
      | obj1==obj2, variableType obj1 == Node ->
      let obj = obj1{variableType = StaticNode (nodeSize alts)} in
      Bind [obj] (Fetch ptr) $
      Case obj (fmap lower mbDefault)
        [ Alternative pattern (lower branch)
        | Alternative pattern branch <- alts ]
    Bind vars expr rest -> Bind vars expr (lower rest)
    Case scrut mbDefault alts ->
      Case scrut (fmap lower mbDefault)
        [ Alternative pattern (lower branch)
        | Alternative pattern branch <- alts ]
    _ -> block
  where
    nodeSize [] = 0
    nodeSize (Alternative (NodePat _ args) _ : xs) = max (1+length args) (nodeSize xs)
    nodeSize (x:xs) = nodeSize xs -- XXX: This shouldn't happen
