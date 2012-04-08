module Grin.HPT.Interface
    ( HeapAnalysis
    , Node
    , IsShared
    , Rhs(..)
    , isSubsetOf
    , mkHeapAnalysis
    , lookupHeap
    , lookupLhs
    , heapIsShared
    , hptIsShared
    , hptSetShared
    , hptAddBinding
    , rhsSize
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Grin.Types           ( Renamed(..), NodeType, uniqueId )
import Grin.HPT.Environment ( HeapPointer, Lhs(..), Node )

import Data.Monoid

import Grin.Stage2.Pretty (ppNodeType)
import Text.PrettyPrint.ANSI.Leijen

import Control.Parallel.Strategies
import Control.DeepSeq

import qualified Data.HashMap.Strict as HT
import qualified Data.Hashable as HT

findWithDefault def key m
  = case HT.lookup key m of
      Nothing  -> def
      Just val -> val

type IsShared = Bool

data Rhs
    = Empty
    | Base
    | Heap {-IntSet.IntSet-} (Set.Set HeapPointer)
    | Other { rhsTagged :: (Map.Map Node [Rhs])
            , rhsVector :: [Rhs] }
--            , rhsHeap   :: Set.Set HeapPointer }
--    | Tagged (Map.Map Node [Rhs])
--    | Vector [Rhs]
--    | Heap (Set.Set HeapPointer)
    deriving (Eq)

instance NFData Rhs where
    rnf Empty = ()
    rnf Base  = ()
    rnf (Heap h) = rnf h
    rnf (Other t v) = if False -- not (Map.null t && null v) && not (Set.null h)
                      then error "Broken invariant."
                      else rnf t `seq` rnf v -- `seq` rnf h

instance NFData Renamed where
    rnf _ = ()
instance NFData NodeType where
    rnf _ = ()

instance Show Rhs where
    showsPrec _ = displayS . renderPretty 1 200 . ppRhs

ppRhs Empty          = text "Empty"
ppRhs Base           = text "Base"
ppRhs (Heap hps)      = text "Heap" <+> list (map int (Set.toList hps))
ppRhs (Other nodes args)
    = text "Other" <+> list [ (ppNodeType nt missing tag) <+> list (map ppRhs args) | ((tag, nt, missing), args) <- Map.toList nodes ]
                   <+> list (map ppRhs args)
--                   <+> list (map int (Set.toList hps))
--ppRhs (Tagged nodes) = 
--ppRhs (Vector args)  = 
--ppRhs (Heap hps)     = 

instance Monoid Rhs where
    mempty = Empty
    mappend = joinRhs

joinRhs :: Rhs -> Rhs -> Rhs
joinRhs Empty rhs                         = rhs
joinRhs rhs Empty                         = rhs
joinRhs Base Base                         = Base
joinRhs (Heap h1) (Heap h2)               = Heap (Set.union h1 h2)
joinRhs (Other t1 v1) (Other t2 v2)
--    | Map.size t1 `seq` Map.size t2 `seq` False = undefined
--    | Set.size h1 `seq` Set.size h2 `seq` False = undefined
    | otherwise = Other (Map.unionWith zipJoin t1 t2) (zipJoin v1 v2) -- (Set.union h1 h2)
joinRhs rhs Base                          = rhs
joinRhs Base rhs                          = rhs
{-
joinRhs (Tagged nodes1) (Tagged nodes2)   = Tagged (Map.unionWith zipJoin nodes1 nodes2)
joinRhs (Vector args1) (Vector args2)     = Vector (zipJoin args1 args2)
joinRhs (Heap hp1) (Heap hp2)             = Heap (Set.union hp1 hp2)
joinRhs left right                        = error $ "Unmatched rhs values: " ++ show (left,right)
-}

isSubsetOf :: Rhs -> Rhs -> Bool
lRhs `isSubsetOf` rRhs
    = worker lRhs rRhs
    where worker Empty y  = True
          worker x Empty  = False
          worker _ Base   = True
          worker Base _   = False
          worker (Heap h1) (Heap h2) = Set.isSubsetOf h1 h2
          worker (Other t1 v1) (Other t2 v2)
              = Map.isSubmapOfBy (\a b -> and (zipWith isSubsetOf a b)) t1 t2 &&
                and (zipWith isSubsetOf v1 v2)
--                && Set.isSubsetOf h1 h2
{-
          worker (Tagged nodes1) (Tagged nodes2)
              = Map.isSubmapOfBy (\a b -> and (zipWith isSubsetOf a b)) nodes1 nodes2
          worker (Vector args1) (Vector args2)
              = and (zipWith isSubsetOf args1 args2)
          worker (Heap hp1) (Heap hp2)
              = Set.isSubsetOf hp1 hp2
          worker Base Base = True
          worker _ _ = False -- should be an error.
-}


zipJoin :: Monoid a => [a] -> [a] -> [a]
zipJoin [] []         = []
zipJoin [] lst        = zipWith mappend (repeat mempty) lst
zipJoin lst []        = zipWith mappend lst (repeat mempty)
zipJoin (x:xs) (y:ys) = mappend x y : zipJoin xs ys


instance HT.Hashable Lhs where
    hash (VarEntry var) = HT.hash var
    hash (HeapEntry hp) = hp

data HeapAnalysis
    = HeapAnalysis { hptBindings   :: HT.HashMap Lhs Rhs
                   , hptSharingMap :: Map.Map Lhs IsShared
                   }
    deriving (Eq)

instance Show HeapAnalysis where
    show (HeapAnalysis binds smap) = unlines ([ unlines [ show lhs, "  " ++ show rhs] | (lhs,rhs) <- HT.toList binds ])

instance NFData HeapAnalysis where
    rnf hpt = rnf (hptBindings hpt) `seq` rnf (hptSharingMap hpt)


mkHeapAnalysis :: Map.Map Lhs Rhs -> Map.Map Lhs IsShared -> HeapAnalysis
mkHeapAnalysis binds smap
    = HeapAnalysis { hptBindings   = HT.fromList (Map.toList binds)
                   , hptSharingMap = smap
                   }

lookupLhs :: Lhs -> HeapAnalysis -> Rhs
lookupLhs lhs hpt
    = findWithDefault Empty lhs (hptBindings hpt)


rhsSize :: Rhs -> Int
rhsSize Empty = 0
rhsSize Base = 1
rhsSize (Heap hp) = 1
rhsSize (Other t v)
    = maximum [ (1 + maximum (0:map length (Map.elems t)))
              , length v ]
{-
rhsSize (Tagged nodes) = 1 + maximum (0:map length (Map.elems nodes))
rhsSize (Vector args) = length args
rhsSize Heap{} = 1
-}

lookupHeap :: Renamed -> HeapAnalysis -> Rhs
lookupHeap var hpt
    = case HT.lookup (VarEntry var) (hptBindings hpt) of
        Just (Heap hp) -> mconcat [ findWithDefault (errMsg pointer) (HeapEntry pointer) (hptBindings hpt) | pointer <- Set.toList hp ]
        Just Empty     -> Empty
        Just rhs       -> error $ "Grin.HPT.Interface.lookupHeap: Invalid rhs: " ++ show (var, rhs)
        Nothing        -> error $ "Grin.HPT.Interface.lookupHeap: Couldn't find lhs: " ++ show var
    where errMsg p = error $ "Grin.HPT.Interface.lookupHeap: Heap value not found: " ++ show p

heapIsShared :: Renamed -> HeapAnalysis -> IsShared
heapIsShared var hpt
    = case HT.lookup (VarEntry var) (hptBindings hpt) of
        Just (Heap hp) -> or [ Map.findWithDefault False (HeapEntry pointer) (hptSharingMap hpt) | pointer <- Set.toList hp ]
        Just Empty     -> False
        Just rhs       -> error $ "Grin.HPT.Interface.heapIsShared: Invalid rhs: " ++ show (var, rhs)
        Nothing        -> error $ "Grin.HPT.Interface.heapIsShared: Couldn't find lhs: " ++ show var

hptIsShared :: Lhs -> HeapAnalysis -> IsShared
hptIsShared lhs hpt
    = Map.findWithDefault False lhs (hptSharingMap hpt)

hptSetShared :: Lhs -> HeapAnalysis -> HeapAnalysis
hptSetShared lhs hpt
    = hpt { hptSharingMap = Map.insert lhs True (hptSharingMap hpt) }

hptAddBinding :: Lhs -> Rhs -> HeapAnalysis -> HeapAnalysis
hptAddBinding lhs rhs hpt
    = case HT.lookup lhs (hptBindings hpt) of
        Nothing  -> hpt { hptBindings = HT.insert lhs rhs (hptBindings hpt) }
        Just old -> let joined = old `mappend` rhs
                    in rnf joined `seq` hpt { hptBindings = HT.insert lhs joined (hptBindings hpt) }

