module Data.Bedrock.Invoke
    ( invokeName, mkInvoke ) where

import           Control.Applicative           (pure, (<$>), (<*>))
import           Control.Monad.State
import qualified Data.Map                      as Map
import Data.List

import           Data.Bedrock
import           Data.Bedrock.Transform
import           Data.Bedrock.Exceptions


invokeName :: [Type] -> Name
invokeName pattern =
    Name [] ("invoke_" ++ patternName) 0
  where
    patternName = map tyKey pattern
    tyKey NodePtr   = 'p'
    tyKey Node      = 'n'
    tyKey Primitive = 'u'

mkInvoke :: Gen ()
mkInvoke = do
    fs <- gets (Map.elems . envFunctions)
    forM_ fs $ \fn -> do
        body' <- traverseExpression fn (fnBody fn)
        pushFunction fn{fnBody = body'}
    let tyPatterns = nub $ sort $
            concatMap (tails . map variableType . fnArguments) fs
    forM_ tyPatterns $ mkInvokeInstance fs

mkInvokeInstance :: [Function] -> [Type] -> Gen ()
mkInvokeInstance fs pattern = do
    thunk <- newVariable "thunk" Node
    args <- forM pattern $ newVariable "arg"
    let blanks = length pattern
        lastN n lst = drop (length lst - n) lst
        dropLast n = reverse . drop n . reverse
    let exhAlternative =
            Alternative
                (NodePat
                    (ConstructorName exhFrameName)
                    [])
                (Panic "Exception handling is temporarily disabled.")
        matchingFunctions =
            [ fn
            | fn <- fs
            , let argTys = map variableType (fnArguments fn)
            , pattern == lastN blanks argTys ]
        invokeFunction fn =
            Alternative
                (NodePat
                    (FunctionName (fnName fn) blanks)
                    (dropLast blanks (fnArguments fn))) $
                TailCall (fnName fn) $
                    dropLast blanks (fnArguments fn) ++ args
        body =
            Case thunk Nothing
                (exhAlternative : map invokeFunction matchingFunctions)
    pushFunction Function
        { fnName = invokeName pattern
        , fnArguments = thunk : args
        , fnBody = body }

traverseExpression :: Function -> Expression -> Gen Expression
traverseExpression origin expr =
    case expr of
        Bind binds simple rest ->
            Bind binds simple <$> traverseExpression origin rest
        Case scrut defaultBranch alternatives ->
            Case scrut
                <$> pure defaultBranch
                <*> mapM (traverseAlternative origin) alternatives
        Invoke obj args -> do
            node <- newVariable "node" Node
            return $
                Bind [node] (Fetch obj) $
                TailCall
                    (invokeName (map variableType args)) (node : args)
        other -> return other

traverseAlternative :: Function -> Alternative -> Gen Alternative
traverseAlternative origin alternative =
    case alternative of
        Alternative pattern branch ->
            Alternative pattern <$> traverseExpression origin branch



