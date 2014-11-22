module Compiler.Genesis where



class Genesis a where
  type Identifier a
  type Declaration a

newIdentifier :: (Genesis a, Monad m) => m (Identifier a)
newIdentifier = undefined

mkBinding :: (Genesis a, Monad m) => Identifier a -> Declaration a -> m ()
mkBinding = undefined

