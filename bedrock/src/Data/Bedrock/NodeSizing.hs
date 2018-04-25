module Data.Bedrock.NodeSizing
    ( lowerNodeSize ) where

import           Control.Applicative     (pure, (<$>), (<*>))
import           Control.Monad.State
import qualified Data.Map                as Map


import           Data.Bedrock
import           Data.Bedrock.HPT
import           Data.Bedrock.Transform


lowerNodeSize :: HPTResult -> Gen ()
lowerNodeSize hpt = do
    fs <- gets (Map.elems . envFunctions)
    forM_ fs $ \fn -> do
        body' <- traverseBlock hpt fn (fnBody fn)
        let rets = Map.lookup (fnName fn) (hptFnRets hpt)
            typeRet = variableType . setVariableSize hpt
        let typedRets = maybe (fnResults fn) (map typeRet) rets
        pushFunction fn
            { fnBody = body'
            , fnArguments = map (setVariableSize hpt) (fnArguments fn)
            , fnResults = typedRets }
    return ()

traverseMaybe :: (a -> Gen a) -> Maybe a -> Gen (Maybe a)
traverseMaybe _ Nothing = return Nothing
traverseMaybe fn (Just val) = Just <$> fn val

traverseBlock :: HPTResult -> Function -> Block -> Gen Block
traverseBlock hpt origin block =
    case block of
        Bind binds simple rest ->
            Bind
                (map (setVariableSize hpt) binds)
                (traverseExpression hpt simple)
                <$> traverseBlock hpt origin rest
        Case scrut defaultBranch alternatives ->
            Case (setVariableSize hpt scrut)
                <$> traverseMaybe (traverseBlock hpt origin) defaultBranch
                <*> mapM (traverseAlternative hpt origin) alternatives
        Return vars ->
            pure $ Return (map (setVariableSize hpt) vars)
        TailCall fn vars ->
            pure $ TailCall fn (map (setVariableSize hpt) vars)
        Invoke{} -> invalid "Invoke"
        -- InvokeHandler{} -> invalid "InvokeHandler"
        Raise{}  -> invalid "Raise"
        Exit -> pure block
        Panic{} -> pure block
  where
    invalid ty = error $ "NodeSizing: Found invalid AST: " ++ ty

traverseExpression :: HPTResult -> Expression -> Expression
traverseExpression hpt expr =
    case expr of
        Store node vars -> Store node (map (setVariableSize hpt) vars)
        Application fn vars -> Application fn (map (setVariableSize hpt) vars)
        TypeCast var -> TypeCast (setVariableSize hpt var)
        _ -> expr

traverseAlternative :: HPTResult -> Function -> Alternative -> Gen Alternative
traverseAlternative hpt origin alternative =
    case alternative of
        Alternative pattern branch ->
            Alternative (traversePattern hpt pattern)
                <$> traverseBlock hpt origin branch

traversePattern :: HPTResult -> Pattern -> Pattern
traversePattern hpt pattern =
    case pattern of
        NodePat node vars -> NodePat node (map (setVariableSize hpt) vars)
        LitPat{} -> pattern
        -- VarPat var -> VarPat (setVariableSize hpt var)
