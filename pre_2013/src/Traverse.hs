module Traverse where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Monoid

class Traverse a where
    tmap   ::             (a -> a) -> a -> a
    tmapM  :: Monad m =>  (a -> m a) -> a -> m a
    tmapM_ :: Monad m =>  (a -> m b) -> a -> m a
    tsum   :: Monoid b => (a -> b) -> a -> b

    tmap fn = runIdentity . tmapM (return . fn)
    tmapM_ fn = tmapM (\a -> fn a >> return a)
    tsum fn = execWriter . tmapM_ (tell . fn)
