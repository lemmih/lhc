{-# LANGUAGE DeriveGeneric #-}
module Language.Haskell.Crux.Interface where

import           Codec.Serialise
import qualified Data.Map                   as Map
import           Data.Maybe
import           GHC.Generics
import           Language.Haskell.Exts      (SrcSpan, SrcSpanInfo)
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
  } deriving (Show, Generic)

mkInterface :: Scope.Interface -> TcEnv -> Interface
mkInterface scope env = Interface
  { ifaceValues       = catMaybes
      [ case Map.lookup entity (tcEnvValues env) of
          Nothing -> error $ "Compiler.Interface.mkInterface: missing type: " ++ show (entityName entity)
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
writeInterface path iface = writeFileSerialise path iface -- writeCompact path =<< compact iface

readInterface :: FilePath -> IO Interface
readInterface path = readFileDeserialise path

instance Serialise Interface
instance Serialise Entity

instance Serialise SrcSpanInfo
instance Serialise SrcSpan
instance Serialise Scope.QualifiedName
instance Serialise Scope.EntityKind
instance Serialise Type
instance Serialise a => Serialise (Qualified a)
instance Serialise Predicate
instance Serialise TcVar
