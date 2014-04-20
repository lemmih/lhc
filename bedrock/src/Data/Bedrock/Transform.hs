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

runGen :: Module -> Gen a -> Module
runGen initModule gen =
    envModule (execState gen m)
  where
    m = Env
        { envModule = initModule{functions = []}
        , envUnique = 0
        , envFunctions = Map.fromList
            [ (fnName fn, fn) | fn <- functions initModule]
        }

runGens :: Module -> [Gen ()] -> Module
runGens m [] = m
runGens m (gen:gens) = runGens (runGen m gen) gens


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



freeVariables :: Expression -> Set Variable
freeVariables expr = freeVariables' expr Set.empty

freeVariables' :: Expression -> Set Variable -> Set Variable
freeVariables' expression =
    case expression of
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
        Throw name ->
            Set.insert name
        Invoke cont args ->
            Set.union (Set.fromList (cont:args))
        TailCall _name args ->
            Set.union (Set.fromList args)
        Exit ->
            id

freeVariablesPattern :: Pattern -> Set Variable -> Set Variable
freeVariablesPattern pattern =
    case pattern of
        NodePat _ vars -> Set.union (Set.fromList vars)
        LitPat{}       -> id

freeVariablesSimple :: SimpleExpression -> Set Variable -> Set Variable
freeVariablesSimple simple =
    case simple of
        Literal{} ->
            id
        Application _fn args ->
            Set.union (Set.fromList args)
        WithExceptionHandler _exh exhArgs _fn fnArgs ->
            Set.union (Set.fromList (exhArgs ++ fnArgs))
        Alloc{} ->
            id
        GCAllocate{} ->
            id
        Store _constructor args ->
            Set.union (Set.fromList args)
        Fetch ptr ->
            Set.insert ptr
        Load ptr _idx ->
            Set.insert ptr
        Add lhs rhs ->
            Set.insert lhs . Set.insert rhs
        Print var ->
            Set.insert var
        Unit args ->
            freeVariablesArguments args
        Eval var ->
            Set.insert var
        Apply obj arg ->
            Set.insert obj . Set.insert arg

freeVariablesArguments :: [Argument] -> Set Variable -> Set Variable
freeVariablesArguments = flip (foldr freeVariablesArgument)

freeVariablesArgument :: Argument -> Set Variable -> Set Variable
freeVariablesArgument arg =
    case arg of
        RefArg name -> Set.insert name
        NodeArg _node args -> Set.union (Set.fromList args)
        LitArg{} -> id

