{-# LANGUAGE TemplateHaskell #-}
module Compiler.Interface where

import Compiler.Core ()
import           Control.Applicative
import           Data.Binary
import           Data.Derive.Binary
import           Data.DeriveTH
import qualified Data.Map                               as Map
import           Language.Haskell.Exts.Annotated.Syntax
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Scope                 (GlobalName (..),
                                                         Origin)
import           Language.Haskell.Scope                 (QualifiedName (..))
import qualified Language.Haskell.Scope                 as Scope
import           Language.Haskell.TypeCheck.Monad
import           Language.Haskell.TypeCheck.Types

data Interface =
  Interface
  { ifaceValues       :: [(GlobalName, TcType)]
    -- ^ Values such as 'length', 'Just', etc
  , ifaceTypes        :: [(GlobalName, [GlobalName])]
    -- ^ Types: (Maybe, [Nothing, Just])
  , ifaceConstructors :: [(GlobalName, [GlobalName])]
    -- ^ Constructors: (Interface, [ifaceValues, ...])
  , ifaceClasses      :: [(GlobalName, [GlobalName])]
    -- ^ Classes: (Show, [show, showList, showsPrec])
  }

{- Hm, I don't really want to use Data.Binary. And I definitely do not want
   to define instances for external types. Oh well. Lemmih 2014-11-9 -}

derive makeBinary ''Interface



mkInterface :: Scope.Interface -> TcEnv -> Interface
mkInterface scope env = Interface
  { ifaceValues       =
      [ case Map.lookup gname (tcEnvValues env) of
          Nothing -> error "Compiler.Interface.mkInterface: missing type"
          Just ty -> (gname, ty)
      | gname <- Scope.ifaceValues scope ]
  , ifaceTypes        = Scope.ifaceTypes scope
  , ifaceConstructors = []
  , ifaceClasses      = [] }

toScopeInterface :: Interface -> Scope.Interface
toScopeInterface iface =
  Scope.Interface
  { Scope.ifaceValues = map fst (ifaceValues iface)
  , Scope.ifaceTypes  = ifaceTypes iface
  , Scope.ifaceConstructors = []
  , Scope.ifaceClasses = []
  }

addToTcEnv :: Interface -> TcEnv -> TcEnv
addToTcEnv iface env = env
  { tcEnvValues = Map.union (Map.fromList (ifaceValues iface)) (tcEnvValues env) }

addAllToTcEnv :: [Interface] -> TcEnv -> TcEnv
addAllToTcEnv [] = id
addAllToTcEnv (x:xs) = addAllToTcEnv xs . addToTcEnv x

writeInterface :: FilePath -> Interface -> IO ()
writeInterface = encodeFile

readInterface :: FilePath -> IO Interface
readInterface = decodeFile






