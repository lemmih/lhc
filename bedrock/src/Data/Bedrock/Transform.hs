module Data.Bedrock.Transform where

import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set

import           Data.Bedrock


data Env = Env
    { envModule    :: Module
    , envUnique    :: Int
    , envFunctions :: Map Name Function }
type Gen a = State Env a

modifyModule :: (Module -> Module) -> Gen ()
modifyModule fn = modify $ \st -> st{envModule = fn (envModule st)}

pushFunction :: Function -> Gen ()
pushFunction fn = modifyModule $ \m -> m{functions = functions m ++ [fn]}

pushNode :: NodeDefinition -> Gen ()
pushNode n = modifyModule $ \m -> m{ nodes = n : nodes m }

lookupAttributes :: Name -> Gen [Attribute]
lookupAttributes name = do
    m <- gets envFunctions
    case Map.lookup name m of
        -- XXX: Throw an exception?
        Nothing -> return []
        Just fn -> return $ fnAttributes fn

hasAttribute :: Name -> Attribute -> Gen Bool
hasAttribute name attr = do
    attrs <- lookupAttributes name
    return $ attr `elem` attrs

newUnique :: Gen Int
newUnique = do
    u <- gets envUnique
    modify $ \st -> st{envUnique = u+1}
    return u

newName :: String -> Gen Name
newName name = do
    u <- newUnique
    return Name
        { nameModule = []
        , nameIdentifier = name
        , nameUnique = u }

newVariable :: String -> Type -> Gen Variable
newVariable ident ty = do
    name <- newName ident
    return Variable
        { variableName = name
        , variableType = ty }

tagName :: String -> Name -> Gen Name
tagName tag name = do
    u <- newUnique
    return $ name{ nameIdentifier = nameIdentifier name ++ "_" ++ tag
                 , nameUnique = u}

tagVariable :: String -> Variable -> Gen Variable
tagVariable tag var = do
    nameTag <- tagName tag (variableName var)
    return var{ variableName = nameTag }



-- FIXME: This is O(n)
--lookupFunction :: Name -> Gen Function
--lookupFunction name = do
--    funcs <- gets envFunctions
--    case Map.lookup name funcs of
--        Just fn -> return fn
--        Nothing -> error $ "Missing function: " ++ show name

runGen :: Gen a -> Module -> Module
runGen gen initModule =
    envModule (execState gen m)
  where
    m = Env
        { envModule = initModule{functions = []}
        , envUnique = 0
        , envFunctions = Map.fromList
            [ (fnName fn, fn) | fn <- functions initModule]
        }

--usedNodes :: Expression -> Set NodeName
--usedNodes = flip usedNodes' Set.empty
--  where
--    usedNodes' expr =
--        case expr of
--            Case _scrut _defaultBranch alternatives ->
--                foldr (.) id
--                [ usedNodes' branch
--                | Alternative _pattern branch <- alternatives ]
--            Bind _ simple rest ->
--                usedNodes' rest



freeVariables :: Block -> Set Variable
freeVariables block = freeVariables' block Set.empty

freeVariables' :: Block -> Set Variable -> Set Variable
freeVariables' block =
    case block of
        Case scrut _defaultBranch alternatives ->
            foldr (.) (Set.insert scrut)
            [ flip Set.difference (freeVariablesPattern pattern Set.empty) .
              freeVariables' branch
            | Alternative pattern branch <- alternatives ]
        Bind binds simple rest ->
            freeVariablesSimple simple .
            flip Set.difference (Set.fromList binds) .
            freeVariables' rest
        Return args ->
            Set.union (Set.fromList args)
        Raise name ->
            Set.insert name
        Invoke cont args ->
            Set.union (Set.fromList (cont:args))
        InvokeHandler cont exception ->
            Set.insert cont . Set.insert exception
        TailCall _name args ->
            Set.union (Set.fromList args)
        Exit ->
            id
        Panic{} ->
            id

freeVariablesPattern :: Pattern -> Set Variable -> Set Variable
freeVariablesPattern pattern =
    case pattern of
        NodePat _ vars -> Set.union (Set.fromList vars)
        LitPat{}       -> id

freeVariablesSimple :: Expression -> Set Variable -> Set Variable
freeVariablesSimple simple =
    case simple of
        Application _fn args ->
            Set.union (Set.fromList args)
        CCall _fn args ->
            Set.union (Set.fromList args)
        Catch _exh exhArgs _fn fnArgs ->
            Set.union (Set.fromList (exhArgs ++ fnArgs))
        Alloc{} ->
            id
        GCAllocate{} ->
            id
        Store _constructor args ->
            Set.union (Set.fromList args)
        Write ptr _idx var ->
            Set.insert ptr . Set.insert var
        Fetch ptr ->
            Set.insert ptr
        Load ptr _idx ->
            Set.insert ptr
        Add lhs rhs ->
            Set.insert lhs . Set.insert rhs
        ReadRegister{} -> id
        WriteRegister _reg var -> Set.insert var
        Address var _idx -> Set.insert var
        TypeCast var -> Set.insert var
        Eval var ->
            Set.insert var
        Apply obj arg ->
            Set.insert obj . Set.insert arg
        ReadGlobal{} -> id
        WriteGlobal _reg var -> Set.insert var
        MkNode _ vars -> Set.union (Set.fromList vars)
        Literal{} -> id
        GCBegin{} -> id
        GCEnd{} -> id
        GCMark var -> Set.insert var
        GCMarkNode var -> Set.insert var

