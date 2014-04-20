{-# LANGUAGE PatternGuards, GeneralizedNewtypeDeriving #-}
module Data.Bedrock.GlobalVariables
    ( lowerGlobalRegisters ) where

import           Control.Applicative    (Applicative,(<$>))
import           Control.Monad.Reader
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import Data.Set ( Set)
import Data.Map (Map)

import           Data.Bedrock
import           Data.Bedrock.Transform ()
import           Data.Bedrock.Misc


lowerGlobalRegisters :: Module -> Module
lowerGlobalRegisters m = m
    { functions = map (runLower . lowerFunction) (functions m)
    , modNamespace = ns }
  where
    runLower action = runReader (unLower action) env
    env = Map.fromList
        [ (name, Variable (Name [] name idNum) Primitive)
        | (idNum, name) <- zip idNums regs ]
    (idNums, ns) = getGlobalIDs (modNamespace m) (length regs)
    regs = Set.toList $ allRegisters m

lowerFunction :: Function -> Lower Function
lowerFunction fn = do
    regs <- asks Map.elems
    body' <- lowerExpression (fnBody fn)
    return fn
        { fnBody = body'
        , fnArguments = regs ++ fnArguments fn }

lowerExpression :: Expression -> Lower Expression
lowerExpression expr =
    case expr of
        -- XXX: For normal calls, save registers in global
        -- variables and restore them after the call.

        Bind [bind] (ReadRegister reg) rest -> do
            var <- asks (Map.! reg)
            rest' <- lowerExpression rest
            return $ Bind [bind] (Unit [RefArg var]) rest'
        Bind [] (WriteRegister reg var) rest ->
            local (Map.insert reg var) $
                lowerExpression rest
        Bind binds simple rest ->
            Bind binds simple <$> lowerExpression rest
        Case scrut defaultBranch alts ->
            Case scrut defaultBranch <$> mapM lowerAlternative alts
        TailCall fn args -> do
            regs <- asks Map.elems
            return $ TailCall fn (regs ++ args)
        Exit -> return Exit
        _ -> error $ "lower registers: " ++ show expr

lowerAlternative :: Alternative -> Lower Alternative
lowerAlternative (Alternative pattern branch) =
    Alternative pattern <$> lowerExpression branch

type Env = Map String Variable
newtype Lower a = Lower { unLower :: Reader Env a }
    deriving (Monad, MonadReader Env, Applicative, Functor)

allRegisters :: Module -> Set String
allRegisters m = foldr (.) id (map byFn (functions m)) Set.empty
  where
    byFn = byExpression . fnBody
    byExpression expr =
        case expr of
            Bind _ (ReadRegister reg) rest ->
                Set.insert reg . byExpression rest
            Bind _ _ rest ->
                byExpression rest
            Case _scrut _defaultBranch alts ->
                foldr (.) id (map byAlt alts)
            _ -> id
    byAlt (Alternative _pattern branch) = byExpression branch

getGlobalIDs :: AvailableNamespace -> Int -> ([Int], AvailableNamespace)
getGlobalIDs = worker []
  where
    worker acc ns 0 = (reverse acc, ns)
    worker acc ns n =
        let (idNum, ns') = newGlobalID ns
        in worker (idNum:acc) ns' (n-1)

