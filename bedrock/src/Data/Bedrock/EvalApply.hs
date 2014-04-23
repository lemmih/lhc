{-# LANGUAGE PatternGuards #-}
module Data.Bedrock.EvalApply ( lowerEvalApply ) where

import           Control.Applicative    (pure, (<$>), (<*>))
import           Control.Monad
import           Control.Monad.State
import qualified Data.IntSet            as IntSet
import qualified Data.Map               as Map
import qualified Data.Vector            as Vector

import           Data.Bedrock
import           Data.Bedrock.HPT
import           Data.Bedrock.Misc
import           Data.Bedrock.Transform

lowerEvalApply :: HPTResult -> Gen ()
lowerEvalApply hpt = do
    fs <- gets (Map.elems . envFunctions)
    forM_ fs $ \fn -> do
        body' <- traverseBlock hpt fn (fnBody fn)
        pushFunction fn
            { fnBody = body'
            }

traverseBlock :: HPTResult -> Function -> Block -> Gen Block
traverseBlock hpt origin expr =
    case expr of
        Bind binds simple rest ->
            Bind binds
                <$> traverseExpression hpt origin binds simple
                <*> traverseBlock hpt origin rest
        Case scrut defaultBranch alternatives ->
            Case scrut
                <$> pure defaultBranch
                <*> mapM (traverseAlternative hpt origin) alternatives
        other -> return other

traverseExpression :: HPTResult -> Function -> [Variable]
               -> Expression -> Gen Expression
traverseExpression hpt origin binds simple =
    case simple of
        Eval var | [bind] <- binds      -> mkEval hpt origin bind var
        Apply obj arg | [bind] <- binds -> mkApply hpt origin bind obj arg
        Application fn args             ->
            return $ Application fn args
        _                               -> return simple


mkEval :: HPTResult -> Function -> Variable -> Variable -> Gen Expression
mkEval hpt origin bind var = do
    evalName <- tagName "eval" (fnName origin)
    arg <- tagVariable "ptr" var

    let ptrs = hptPtrScope hpt Vector.! variableIndex var
        objects = mergeObjectList $ map (hptHeap hpt Vector.!) (IntSet.toList ptrs)
        maxArgs = maximum (map Vector.length (Map.elems objects))
    preEvalObject <- newVariable "node" (StaticNode (maxArgs+1))
    evalRet <- newName "ret"
    let body =
            Bind [preEvalObject] (Fetch arg) $
            Case preEvalObject Nothing (map mkAlt (Map.keys objects))
        mkAlt name@(FunctionName fn 0) =
            let args = hptFnArgs hpt Map.! fn in
            Alternative (NodePat name args) $
            TailCall fn args
        mkAlt name@(FunctionName fn n) =
            let args = reverse . drop n . reverse $ hptFnArgs hpt Map.! fn in
            Alternative (NodePat name args) $
            Return [preEvalObject]
        mkAlt name@(ConstructorName con) =
            let args = hptNodeArgs hpt Map.! con
                ret = Variable evalRet (StaticNode (length args+1)) in
            Alternative (NodePat name args) $
            Bind [ret] (Unit (NodeArg name args)) $
            Return [ret]
    pushFunction Function
        { fnName = evalName
        , fnArguments = [arg]
        , fnResults = [StaticNode (sizeOfVariable hpt bind)]
        , fnBody = body }
    return $ Application evalName [var]

mkApply :: HPTResult -> Function -> Variable -> Variable -> Variable
        -> Gen Expression
mkApply hpt origin bind obj arg = do
    applyName <- tagName "apply" (fnName origin)
    applyObj <- newVariable "node" (StaticNode (sizeOfVariable hpt obj))
    applyArg <- tagVariable "ptr" arg
    applyRet <- newVariable "ret" (StaticNode (sizeOfVariable hpt bind))
    let objects = hptNodeScope hpt Vector.! variableIndex obj
        names = Map.keys objects
        body =
            Case applyObj Nothing (map mkAlt names)
        mkAlt name@(FunctionName fn 1) =
            let args = init (hptFnArgs hpt Map.! fn) in
            Alternative (NodePat name args) $
            TailCall fn (args ++ [applyArg])
        mkAlt name@(FunctionName fn n) =
            let args = dropLast n $ hptFnArgs hpt Map.! fn in
            Alternative (NodePat name args) $
            Bind [applyRet] (Unit (NodeArg (FunctionName fn (n-1)) (args++[applyArg]))) $
            Return [applyRet]
        mkAlt _ = error "mkApply"
    pushFunction Function
        { fnName = applyName
        , fnArguments = [applyObj, applyArg]
        , fnResults = [StaticNode (sizeOfVariable hpt bind)]
        , fnBody = body }
    return $ Application applyName [obj, arg]


traverseAlternative :: HPTResult -> Function -> Alternative -> Gen Alternative
traverseAlternative hpt origin alternative =
    case alternative of
        Alternative pattern branch ->
            Alternative pattern <$> traverseBlock hpt origin branch


