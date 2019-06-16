module Tigr.Monad where

import Tigr.Types

import qualified Data.Map as Map
import           Control.Exception    (Exception (..), SomeException (..),
                                       handle, throwIO)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.IORef


newtype M a = M { unM :: Context -> Context -> IO a }

instance Functor M where
  fmap f (M g) = M $ \global local -> fmap f (g global local)

instance Applicative M where
  pure a = M $ \_ _ -> pure a
  f <*> g = M $ \global local -> unM f global local <*> unM g global local

instance Monad M where
  f >>= g = M $ \global local -> unM f global local >>= \a -> unM (g a) global local

instance MonadIO M where
  liftIO io = M $ \_global _local -> io

askLocal :: M Context
askLocal = M $ \_global local -> pure local

askGlobal :: M Context
askGlobal = M $ \global _local -> pure global

withLocal :: Context -> M a -> M a
withLocal local f = M $ \global _local -> unM f global local

modifyLocal :: (Context -> Context) -> M a -> M a
modifyLocal m f = M $ \global local -> unM f global (m local)

lookupName :: Name -> M (IORef Thunk)
lookupName name = do
  local <- askLocal
  global <- askGlobal
  case Map.lookup name local of
    Nothing      ->
      case Map.lookup name global of
        Nothing -> error $ "Missing name: " ++ name
        Just closure -> pure closure
    Just closure -> pure closure

handleM :: (CodeException -> M a) -> M a -> M a
handleM handler f = M $ \global local ->
  handle (\e -> unM (handler e) global Map.empty) (unM f global local)

toClosure :: [Name] -> GCode -> M Closure
toClosure vars code = do
  thunks <- mapM lookupName vars
  return $ Closure (Map.fromList $ zip vars thunks) code

runWithClosure :: Closure -> (GCode -> M a) -> M a
runWithClosure (Closure bound gcode) fn = withLocal bound (fn gcode)
