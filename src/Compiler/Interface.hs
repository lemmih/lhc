{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Compiler.Interface where

import Data.Data
import GHC.Generics
import           Compiler.Core              ()
import           Data.Binary
-- import           Data.Derive.Binary
-- import           Data.DeriveTH
import qualified Data.Map                   as Map
import           Data.Maybe
import           Language.Haskell.Scope     (Entity (..))
import qualified Language.Haskell.Scope     as Scope
import           Language.Haskell.TypeCheck as TC

data Interface =
  Interface
  { ifaceValues       :: [(Entity, TC.Type)]
    -- ^ Values such as 'length', 'Just', etc
  , ifaceTypes        :: [(Entity, [Entity])]
    -- ^ Types: (Maybe, [Nothing, Just])
  , ifaceConstructors :: [(Entity, [Entity])]
    -- ^ Constructors: (Interface, [ifaceValues, ...])
  , ifaceClasses      :: [(Entity, [Entity])]
    -- ^ Classes: (Show, [show, showList, showsPrec])
  } deriving (Show, Data, Generic)

instance Binary Interface

{- Hm, I don't really want to use Data.Binary. And I definitely do not want
   to define instances for external types. Oh well. Lemmih 2014-11-9 -}

-- derive makeBinary ''Interface



mkInterface :: Scope.Interface -> TcEnv -> Interface
mkInterface scope env = Interface
  { ifaceValues       = catMaybes
      [ case Map.lookup entity (tcEnvValues env) of
          Nothing -> Nothing -- error $ "Compiler.Interface.mkInterface: missing type: " ++ show (entityName entity)
          Just ty -> Just (entity, ty)
      | entity <- scope ]
  , ifaceTypes        = []
  , ifaceConstructors = []
  , ifaceClasses      = [] }

-- FIXME
toScopeInterface :: Interface -> Scope.Interface
toScopeInterface iface = map fst (ifaceValues iface)

addToTcEnv :: Interface -> TcEnv -> TcEnv
addToTcEnv iface env = env
  { tcEnvValues = Map.union (Map.fromList (ifaceValues iface)) (tcEnvValues env) }

addAllToTcEnv :: [Interface] -> TcEnv -> TcEnv
addAllToTcEnv []     = id
addAllToTcEnv (x:xs) = addAllToTcEnv xs . addToTcEnv x

writeInterface :: FilePath -> Interface -> IO ()
writeInterface = encodeFile

readInterface :: FilePath -> IO Interface
readInterface = decodeFile
