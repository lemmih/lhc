module Data.Bedrock.Invoke
    ( mkInvoke ) where

import           Control.Applicative     (pure, (<$>), (<*>))
import           Control.Monad.State
import qualified Data.Map                as Map
import qualified Data.Vector             as Vector


import           Data.Bedrock
import           Data.Bedrock.HPT
import           Data.Bedrock.Misc
import           Data.Bedrock.Transform


mkInvoke :: HPTResult -> Gen ()
mkInvoke hpt = do
    fs <- gets (Map.elems . envFunctions)
    forM_ fs $ \fn -> do
        body' <- traverseExpression hpt fn (fnBody fn)
        pushFunction fn{fnBody = body'}
    return ()


traverseExpression :: HPTResult -> Function -> Expression -> Gen Expression
traverseExpression hpt origin expr =
    case expr of
        Bind binds simple rest ->
            Bind (map (setVariableSize hpt) binds) simple <$> traverseExpression hpt origin rest
        Case scrut defaultBranch alternatives ->
            Case scrut
                <$> pure defaultBranch
                <*> mapM (traverseAlternative hpt origin) alternatives
        Invoke obj args -> do
            let Right objects = hptScope hpt Vector.! variableIndex obj
                names = Map.keys objects
                mkAlt name@(FunctionName fn blanks) | blanks == length args =
                    let partialArgs = dropLast blanks $ hptFnArgs hpt Map.! fn in
                    Alternative (NodePat name partialArgs) $
                    TailCall fn (map (setVariableSize hpt) $ partialArgs ++ args)
            return $
                Case (setVariableSize hpt obj) Nothing (map mkAlt names)
        other -> return other

traverseAlternative :: HPTResult -> Function -> Alternative -> Gen Alternative
traverseAlternative hpt origin alternative =
    case alternative of
        Alternative pattern branch ->
            Alternative pattern <$> traverseExpression hpt origin branch



