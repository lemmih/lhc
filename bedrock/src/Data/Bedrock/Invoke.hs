module Data.Bedrock.Invoke
    ( lowerInvoke ) where

import           Control.Applicative     (pure, (<$>), (<*>))
import           Control.Monad.State
import qualified Data.Map                as Map
import qualified  Data.IntSet as IntSet
import qualified Data.Vector             as Vector
import Data.List

import           Data.Bedrock
import           Data.Bedrock.HPT
import           Data.Bedrock.Misc
import           Data.Bedrock.Transform
import           Data.Bedrock.Exceptions (isCatchFrame)


-- Lower calls to @Invoke and @InvokeHandler
lowerInvoke :: HPTResult -> Gen ()
lowerInvoke hpt = do
    fs <- gets (Map.elems . envFunctions)
    forM_ fs $ \fn -> do
        body' <- traverseBlock hpt fn (fnBody fn)
        pushFunction fn
            { fnBody = body'
            }
    return ()


traverseBlock :: HPTResult -> Function -> Block -> Gen Block
traverseBlock hpt origin block =
    case block of
        Bind binds simple rest ->
            Bind
                (map (setVariableSize hpt) binds)
                (traverseExpression hpt simple)
                <$> traverseBlock hpt origin rest
        Case scrut defaultBranch alternatives ->
            Case scrut
                <$> pure defaultBranch
                <*> mapM (traverseAlternative hpt origin) alternatives
        Invoke obj args ->
            Case (setVariableSize hpt obj) Nothing
                <$> mkInvokeAlts hpt (hptNodeScope hpt Vector.! variableIndex obj) args
        InvokeHandler obj arg ->
            Case (setVariableSize hpt obj) Nothing
                <$> mkInvokeHandlerAlts hpt (hptNodeScope hpt Vector.! variableIndex obj) arg
        other -> return other

traverseExpression :: HPTResult -> Expression -> Expression
traverseExpression hpt expr =
    case expr of
        Store node vars -> Store node (map (setVariableSize hpt) vars)
        _ -> expr

derefPtrs :: HPTResult -> NameSet -> HeapPtrSet
derefPtrs hpt ptrs = IntSet.unions
    [ hptPtrScope hpt Vector.! ptr
    | ptr <- IntSet.toList ptrs ]

derefVars :: HPTResult -> NameSet -> Objects
derefVars hpt vars = mergeObjectList
    [ hptNodeScope hpt Vector.! var
    | var <- IntSet.toList vars ]

derefHeap :: HPTResult -> HeapPtrSet -> Objects
derefHeap hpt vars = mergeObjectList
    [ hptHeap hpt Vector.! hp
    | hp <- IntSet.toList vars ]

mkInvokeAlts :: HPTResult -> Objects -> [Variable] -> Gen [Alternative]
mkInvokeAlts hpt objects args = do
    let mkAlt (name@(FunctionName fn blanks), _objArgs)
                | blanks == length args = do
            let partialArgs = dropLast blanks $ hptFnArgs hpt Map.! fn
            return $
                Alternative (NodePat name partialArgs) $
                TailCall fn (map (setVariableSize hpt) $
                    partialArgs ++ args)
        mkAlt (ConstructorName cons, objArgs) | isCatchFrame cons = do
            let nextFramePtrs = objArgs Vector.! 0
                nextFrameLocs = derefPtrs hpt nextFramePtrs
                nextFrameObjects = derefHeap hpt nextFrameLocs
            nextFramePtr <- newVariable "nextFrame" NodePtr
            nextFrame <- newVariable "nextFrame" (StaticNode (sizeOfObjects hpt nextFrameObjects))
            exhPtr <- newVariable "exhPtr" Node
            Alternative (NodePat (ConstructorName cons) [nextFramePtr, exhPtr]) .
                Bind [nextFrame] (Fetch nextFramePtr) .
                Case nextFrame Nothing
                    <$> mkInvokeAlts hpt nextFrameObjects args

        mkAlt (alt, _args) =
            return $ Alternative (NodePat alt []) (Panic "Invoke failure")

    mapM mkAlt (Map.toList objects)

mkInvokeHandlerAlts :: HPTResult -> Objects -> Variable -> Gen [Alternative]
mkInvokeHandlerAlts hpt objects arg = do
    let mkAlt (name@(FunctionName fn blanks), objArgs) = do
            let partialArgs = dropLast blanks $ hptFnArgs hpt Map.! fn
                isFrameVariable var = variableType var == FramePtr
            case findIndex isFrameVariable partialArgs of
                Nothing ->
                    return $
                        Alternative (NodePat name partialArgs) $
                        Panic "Couldn't find frame pointer."
                Just framePtrIndex -> do
                    let nextFramePtrs = objArgs Vector.! framePtrIndex
                        nextFrameLocs = derefPtrs hpt nextFramePtrs
                        nextFrameObjects = derefHeap hpt nextFrameLocs
                        size = sizeOfObjects hpt nextFrameObjects
                    let nextFramePtr = partialArgs !! framePtrIndex
                    nextFrame <- newVariable "nextFrame" (StaticNode size)
                    Alternative (NodePat name partialArgs) .
                        Bind [nextFrame] (Fetch nextFramePtr) .
                        Case nextFrame Nothing
                            <$> mkInvokeHandlerAlts hpt nextFrameObjects arg
        mkAlt (ConstructorName cons, objArgs) | isCatchFrame cons = do
            let exhVars = objArgs Vector.! 1
                exhObjects = derefVars hpt exhVars
            nextFramePtr <- newVariable "nextFrame" NodePtr
            exh <- newVariable "exh" (StaticNode (sizeOfObjects hpt exhObjects))
            Alternative (NodePat (ConstructorName cons) [nextFramePtr, exh]) .
                Case exh Nothing
                    <$> mkInvokeAlts hpt exhObjects [arg, nextFramePtr]

        mkAlt (alt, _args) =
            return $ Alternative (NodePat alt []) (Panic "Invoke handler failure")

    mapM mkAlt (Map.toList objects)


traverseAlternative :: HPTResult -> Function -> Alternative -> Gen Alternative
traverseAlternative hpt origin alternative =
    case alternative of
        Alternative pattern branch ->
            Alternative pattern <$> traverseBlock hpt origin branch



