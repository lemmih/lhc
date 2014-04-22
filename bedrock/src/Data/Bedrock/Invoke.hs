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
        body' <- traverseBlock hpt fn (fnBody fn)
        pushFunction fn{fnBody = body'}
    return ()


traverseBlock :: HPTResult -> Function -> Block -> Gen Block
traverseBlock hpt origin block =
    case block of
        Bind binds simple rest ->
            Bind (map (setVariableSize hpt) binds) simple <$>
                traverseBlock hpt origin rest
        Case scrut defaultBranch alternatives ->
            Case scrut
                <$> pure defaultBranch
                <*> mapM (traverseAlternative hpt origin) alternatives
        Invoke obj args -> do
            let objects = hptNodeScope hpt Vector.! variableIndex obj
                names = Map.keys objects
                mkAlt name@(FunctionName fn blanks) | blanks == length args =
                    let partialArgs = dropLast blanks $ hptFnArgs hpt Map.! fn in
                    Alternative (NodePat name partialArgs) $
                    TailCall fn (map (setVariableSize hpt) $ partialArgs ++ args)
                mkAlt alt = 
                    Alternative (NodePat alt []) Exit
                    --error $ "invoke alt: " ++ show (alt, fnName origin)
            return $
                Case (setVariableSize hpt obj) Nothing (map mkAlt names)
        other -> return other

traverseAlternative :: HPTResult -> Function -> Alternative -> Gen Alternative
traverseAlternative hpt origin alternative =
    case alternative of
        Alternative pattern branch ->
            Alternative pattern <$> traverseBlock hpt origin branch



