{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Bedrock.Simplify ( simplify ) where

import           Control.Applicative ( Applicative, (<$>), (<*>), pure)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map               (Map)
import qualified Data.Map               as Map

import           Data.Bedrock
import           Data.Bedrock.Misc

data Env = Env
    { envRenaming  :: Map Variable Variable
    }
newtype M a = M { unM :: ReaderT Env (State AvailableNamespace) a }
    deriving
        ( Monad, MonadReader Env, Applicative, Functor
        , MonadState AvailableNamespace )

simplify :: Module -> Module
simplify m = runM (simplifyModule m)
  where
    runM action = evalState (runReaderT (unM action) env) st
    st = modNamespace m
    env = Env
        { envRenaming = Map.empty }

simplifyModule :: Module -> M Module
simplifyModule m = do
    fns <- mapM simplifyFunction (functions m)
    ns <- get
    return m{ functions = fns, modNamespace = ns }

simplifyFunction :: Function -> M Function
simplifyFunction fn = do
    body <- simplifyExpression (fnBody fn)
    return fn{fnBody = body}

simplifyExpression :: Expression -> M Expression
simplifyExpression expr =
    case expr of
        Case scrut _mbDefaultBranch
                [Alternative (NodePat node []) branch] -> do
            clone <- cloneVariable scrut
            bindVariable scrut clone $
                Bind [clone] (Unit (NodeArg node [])) <$>
                    simplifyExpression branch
        Bind [] Unit{} rest ->
            simplifyExpression rest
        Bind [bind] (Unit arg) rest ->
            Bind [bind] (Unit arg) <$> simplifyExpression rest
        Bind binds simple rest ->
            Bind <$> mapM resolve binds
                <*> pure simple
                <*> simplifyExpression rest
        Case scrut defaultBranch alts ->
            Case
                <$> resolve scrut
                <*> pure defaultBranch
                <*> mapM simplifyAlternative alts
        _ -> return expr

simplifyAlternative :: Alternative -> M Alternative
simplifyAlternative (Alternative pattern branch) =
    Alternative pattern
        <$> simplifyExpression branch





--------------------------------------------------------
-- Utils

cloneVariable :: Variable -> M Variable
cloneVariable var = do
    ns <- get
    let (idNum, ns') = newIDByType ns (variableType var)
        name = (variableName var){ nameUnique = idNum }
    put ns'
    return var{ variableName = name }

resolve :: Variable -> M Variable
resolve var = do
    m <- asks envRenaming
    return $ Map.findWithDefault var var m

_resolveArgument :: Argument -> M Argument
_resolveArgument (RefArg ref) = RefArg <$> resolve ref
_resolveArgument arg = return arg

bindVariable :: Variable -> Variable -> M a -> M a
bindVariable old new = local $ \env ->
    env{ envRenaming = Map.insert old new (envRenaming env) }    

