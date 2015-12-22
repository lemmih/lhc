{-# LANGUAGE Haskell2010 #-}
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
        Bind [node] (Apply obj arg) (Return [retNode]) | node == retNode ->
            mkInlineApply hpt obj arg
        Bind [node] (Eval var) (Return [retNode]) | node == retNode ->
            mkInlineEval hpt var
        Bind binds simple rest ->
            traverseExpression hpt origin binds simple =<<
                traverseBlock hpt origin rest
        Case scrut defaultBranch alternatives ->
            Case scrut
                <$> maybe (return Nothing) (fmap Just . traverseBlock hpt origin) defaultBranch
                <*> mapM (traverseAlternative hpt origin) alternatives
        other -> return other

traverseExpression :: HPTResult -> Function -> [Variable]
               -> Expression -> Block -> Gen Block
traverseExpression hpt origin binds simple rest =
    case simple of
        Eval var | [bind] <- binds      -> mkEval hpt origin bind var rest
        Apply obj arg | [bind] <- binds -> mkApply hpt origin bind obj arg rest
        _                               -> return $ Bind binds simple rest


isSimpleEval :: HPTResult -> Variable -> Bool
isSimpleEval hpt var =
    all isWhnf (Map.keys objects)
  where
    HeapLocationSet ptrs = hptPtrScope hpt Vector.! variableIndex var
    objects = mergeObjectList $ map (hptHeap hpt Vector.!) (IntSet.toList ptrs)
    isWhnf (FunctionName _ 0) = False
    isWhnf _ = True

mkEvalBody :: HPTResult -> Int -> Variable -> Gen (Variable, Variable, Block)
mkEvalBody hpt retSize var = do
    arg <- tagVariable "ptr" var

    let HeapLocationSet ptrs = hptPtrScope hpt Vector.! variableIndex var
        objects = mergeObjectList $ map (hptHeap hpt Vector.!) (IntSet.toList ptrs)
        maxArgs = foldr max 0 (map Vector.length (Map.elems objects))
    preEvalObject <- newVariable "node" (StaticNode (maxArgs+1))
    evalRet <- newName "ret"
    let body = Case preEvalObject Nothing (map mkAlt (Map.keys objects))
        mkAlt name@(FunctionName fn 0) =
            let args = hptFnArgs hpt Map.! fn
                [retVar] = hptFnRets hpt Map.! fn
                branchRetSize = sizeOfVariable hpt retVar
                ret = Variable evalRet (StaticNode branchRetSize) in
            Alternative (NodePat name args) $
            if branchRetSize == retSize
                then TailCall fn args
                else Bind [ret] (Application fn args) (Return [ret])
        mkAlt name@(FunctionName fn n) =
            let args = reverse . drop n . reverse $ hptFnArgs hpt Map.! fn
                ret = Variable evalRet (StaticNode (length args+1)) in
            Alternative (NodePat name args) $
            Bind [ret] (MkNode name args) $
            Return [ret]
            -- Alternative (NodePat name args) $
            -- Return [preEvalObject{variableType = StaticNode (length args+1)}]
        mkAlt name@(ConstructorName con blanks) =
            let args = dropLast blanks $ hptNodeArgs hpt Map.! con
                ret = Variable evalRet (StaticNode (length args+1)) in
            Alternative (NodePat name args) $
            Bind [ret] (MkNode name args) $
            Return [ret]
    return (arg, preEvalObject, body)

mkEval :: HPTResult -> Function -> Variable -> Variable -> Block -> Gen Block
mkEval hpt origin bind var rest | isSimpleEval hpt var = do
    return $
        Bind [bind{variableType = StaticNode (sizeOfVariable hpt bind)}]
            (Fetch constantMemory var)
            rest
mkEval hpt origin bind var rest = do
    evalName <- tagName "eval" (fnName origin)
    (arg, preEvalObject, body) <- mkEvalBody hpt (sizeOfVariable hpt bind) var

    pushFunction Function
        { fnName = evalName
        , fnAttributes = []
        , fnArguments = [arg, preEvalObject]
        , fnResults = [StaticNode (sizeOfVariable hpt bind)]
        , fnBody = body }
    return $
        Bind [preEvalObject] (Fetch anyMemory var) $
        Bind [bind] (Application evalName [var, preEvalObject])
        rest

mkInlineEval :: HPTResult -> Variable -> Gen Block
mkInlineEval hpt var = do
    (arg, preEvalObject, body) <- mkEvalBody hpt 0 var
    return $
        Bind [arg] (TypeCast var) $
        Bind [preEvalObject] (Fetch anyMemory var) $
        body


mkApplyBody :: HPTResult -> Variable -> Variable
        -> Gen (Variable, Variable, Block)
mkApplyBody hpt obj arg = do
    applyObj <- newVariable "node" (StaticNode (sizeOfVariable hpt obj))
    applyArg <- tagVariable "arg" arg
    applyRet <- newName "ret"
    let objects = hptNodeScope hpt Vector.! variableIndex obj
        names = Map.keys objects
        body =
            Case applyObj Nothing (map mkAlt names)
        mkAlt name@(FunctionName fn 1) =
            let args = init (hptFnArgs hpt Map.! fn) in
            Alternative (NodePat name args) $
            TailCall fn (args ++ [applyArg])
        mkAlt name@(FunctionName fn n) =
            let args = dropLast n $ hptFnArgs hpt Map.! fn
                ret = Variable applyRet (StaticNode (length args+1+1)) in
            Alternative (NodePat name args) $
            Bind [ret] (MkNode (FunctionName fn (n-1)) (args++[applyArg])) $
            Return [ret]
        mkAlt name@(ConstructorName fn n) =
            let args = dropLast n $ hptNodeArgs hpt Map.! fn
                ret = Variable applyRet (StaticNode (length args+1+1)) in
            Alternative (NodePat name args) $
            Bind [ret] (MkNode (ConstructorName fn (n-1)) (args++[applyArg])) $
            Return [ret]
        mkAlt name = error $ "mkApply: " ++ show name
    return (applyObj, applyArg, body)

mkApply :: HPTResult -> Function -> Variable -> Variable -> Variable
        -> Block -> Gen Block
mkApply hpt origin bind obj arg rest = do
    applyName <- tagName "apply" (fnName origin)
    (applyObj, applyArg, body) <- mkApplyBody hpt obj arg

    applyRet <- newVariable "ret" (StaticNode (sizeOfVariable hpt bind))
    pushFunction Function
        { fnName = applyName
        , fnAttributes = []
        , fnArguments = [applyObj, applyArg]
        , fnResults = [StaticNode (sizeOfVariable hpt bind)]
        , fnBody = body }
    return $ Bind [bind] (Application applyName [obj, arg]) rest

mkInlineApply :: HPTResult -> Variable -> Variable -> Gen Block
mkInlineApply hpt obj arg = do
    (applyObj, applyArg, body) <- mkApplyBody hpt obj arg
    return $
        Bind [applyObj] (TypeCast obj) $
        Bind [applyArg] (TypeCast arg) $
        body

traverseAlternative :: HPTResult -> Function -> Alternative -> Gen Alternative
traverseAlternative hpt origin alternative =
    case alternative of
        Alternative pattern branch ->
            Alternative pattern <$> traverseBlock hpt origin branch


