module Grin.HPT
    ( analyze
    , lower
    , mkEnvironment
    , showEquations
    , module Grin.HPT.Interface
    , Lhs (..)
    ) where

import Grin.Types               ( Grin )

import Grin.HPT.Environment     ( mkEnvironment, Lhs(..), showEquations )
--import Grin.HPT.Solve           ( )
import Grin.HPT.QuickSolve      ( solve )
--import Grin.HPT.FastSolve       ( ) -- BROKEN.
import Grin.HPT.Lower           ( lower )
import Grin.HPT.Interface

analyze :: Grin -> [HeapAnalysis]
analyze = solve . mkEnvironment

