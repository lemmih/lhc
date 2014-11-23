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
    , envConstant  :: Map Variable (NodeName, [Variable])
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
        { envRenaming = Map.empty
        , envConstant = Map.empty }

simplifyModule :: Module -> M Module
simplifyModule m = do
    fns <- mapM simplifyFunction (functions m)
    ns <- get
    return m{ functions = fns, modNamespace = ns }

simplifyFunction :: Function -> M Function
simplifyFunction fn = do
    body <- simplifyBlock (fnBody fn)
    return fn{fnBody = body}

simplifyBlock :: Block -> M Block
simplifyBlock block =
    case block of
        {-Case scrut _mbDefaultBranch
                [Alternative (NodePat node []) branch] -> do
            clone <- cloneVariable scrut
            bindVariable scrut clone $
                Bind [clone] (Unit (NodeArg node [])) <$>
                    simplifyBlock branch-}
        Bind [node] expr@(MkNode name vars) rest ->
            bindConstant node name vars $
            Bind [node]
                <$> simplifyExpression expr
                <*> simplifyBlock rest
        Bind [ret] (Apply node arg) rest -> do
            mbConst <- lookupConstant node
            case mbConst of
                Just (FunctionName fnName 1, args) ->
                    simplifyBlock $
                        Bind [ret] (Application fnName (args ++ [arg])) rest
                Just (FunctionName fnName n, args) | n > 1 ->
                    simplifyBlock $
                        Bind [ret] (MkNode (FunctionName fnName (n-1)) (args ++ [arg])) rest
                _Nothing -> Bind [ret] (Apply node arg) <$> simplifyBlock rest
        Bind [] TypeCast{} rest ->
            simplifyBlock rest
        Bind [dst] (TypeCast src) rest | variableType dst == variableType src ->
            Bind [dst] (TypeCast src) <$>
              bindVariable dst src (simplifyBlock rest)
        Bind binds simple rest ->
            Bind <$> mapM resolve binds
                <*> simplifyExpression simple
                <*> simplifyBlock rest
        Case scrut defaultBranch alts ->
            Case
                <$> resolve scrut
                <*> pure defaultBranch
                <*> mapM simplifyAlternative alts
        Return vars -> Return <$> mapM resolve vars
        TailCall fnName vars -> TailCall fnName <$> mapM resolve vars
        Exit -> pure Exit
        Panic msg -> pure $ Panic msg
        -- _ -> return block

simplifyAlternative :: Alternative -> M Alternative
simplifyAlternative (Alternative pattern branch) =
    Alternative pattern <$> simplifyBlock branch


simplifyExpression :: Expression -> M Expression
simplifyExpression expr =
    case expr of
        Application fn vars -> Application fn <$> mapM resolve vars
        Eval var -> Eval <$> resolve var
        MkNode name vars -> MkNode name <$> mapM resolve vars
        TypeCast var -> TypeCast <$> resolve var
        _ -> pure expr


--------------------------------------------------------
-- Utils

_cloneVariable :: Variable -> M Variable
_cloneVariable var = do
    ns <- get
    let (idNum, ns') = newIDByType ns (variableType var)
        name = (variableName var){ nameUnique = idNum }
    put ns'
    return var{ variableName = name }

resolve :: Variable -> M Variable
resolve var = do
    m <- asks envRenaming
    return $ Map.findWithDefault var var m

bindVariable :: Variable -> Variable -> M a -> M a
bindVariable old new = local $ \env ->
    env{ envRenaming = Map.insert old new (envRenaming env) }

lookupConstant :: Variable -> M (Maybe (NodeName, [Variable]))
lookupConstant var = do
    var' <- resolve var
    m <- asks envConstant
    return $ Map.lookup var' m

bindConstant :: Variable -> NodeName -> [Variable] -> M a -> M a
bindConstant var node vars = local $ \env ->
    env{ envConstant = Map.insert var (node, vars) (envConstant env) }

