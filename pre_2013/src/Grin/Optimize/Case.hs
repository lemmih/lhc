{-# LANGUAGE OverloadedStrings #-}
module Grin.Optimize.Case
    ( optimize
    ) where

import Grin.Types
import Traverse
import Grin.Transform

import Control.Monad.State
import Control.Monad.Reader
import Data.Monoid
import Debug.Trace
import Text.Printf
import Control.Applicative

import qualified Data.Map as Map

optimize :: Grin -> Grin
optimize = id -- runTrans (transformExp caseUnion >> transformExp caseSplit)



{-
do a <- case b of A -> a'
                  B -> b'
   case b of A -> a''
             B -> b''
   c
===>
do a <- case b of A -> new <- a'; a''[a->new]; unit new
                  B -> new' <- b'; b''[a->new']; unit new'
   c
-}
caseUnion :: Expression -> Transform Expression
caseUnion exp
    = case exp of
       Case scut alts :>>= var :-> Case scut' alts' | scut == scut'
         -> do newAlts <- sequence [ unionAlt var alt alts' | alt <- alts ]
               caseUnion $ Case scut newAlts
       Case scut alts :>>= var :-> Case scut' alts' :>> e | scut == scut'
         -> do newAlts <- sequence [ unionAlt var alt alts' | alt <- alts ]
               caseUnion $ Case scut newAlts :>>= var :-> e
       _other
         -> tmapM caseUnion exp

unionAlt var (cond :> exp) alts
    = do new <- newVariableFrom var
         otherExp <- renameExp (Map.singleton var new) (findBranch alts)
         let newBranch = exp :>>= new :-> otherExp :>> Unit (Variable new)
         return $ cond :> newBranch
    where findBranch = foldr findBranchCheck unreachable
          findBranchCheck (c :> branch) continue
              | c == cond = branch   -- Found the matching branch, stop looping.
              | otherwise = continue -- No match, continue looking.
          unreachable = Application (Builtin "unreachable") []
         

{-
do d <- case a of A -> b
                  B -> c
   e
===>
do case a of A -> new <- b; fn args[d->new]
             B -> new' <- c; fn args[d->new']
-}
caseSplit :: Expression -> Transform Expression
caseSplit exp 
    = case exp of
        Case scut alts :>>= var :-> e
          -> do e' <- hoistToTopLevel (Builtin "noname") e
                alts' <- forM alts $ \(cond :> branch) -> do new <- newVariableFrom var
                                                             e'' <- renameExp (Map.singleton var new) e'
                                                             return $ cond :> (branch :>>= new :-> e'')
                caseSplit $ Case scut alts'
        _other
          -> tmapM caseSplit exp

