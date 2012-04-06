{-# LANGUAGE OverloadedStrings #-}
module Grin.HPT.Environment
    ( mkEnvironment
    , Equations
    , showEquations
    , Rhs(..)
    , RhsValue(..)
    , HeapPointer
    , Lhs(..)
    , Node
    , singleton
    , isSubsetOf
    ) where

import CompactString
import Grin.Types hiding (Update)
import qualified Grin.Types as Grin

import qualified Data.Map as Map
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Control.Parallel.Strategies

type HeapPointer = Int
data Lhs = HeapEntry HeapPointer
         | VarEntry Renamed
    deriving (Eq,Ord,Show)

instance NFData Lhs where
    rnf (HeapEntry hp) = ()
    rnf (VarEntry r) = ()

type Node = (Renamed, NodeType, Int) -- Name, node type, missing arguments.

data RhsValue
    = Extract Renamed Node Int
    | ExtractVector Renamed Int
    | Eval Renamed
    | Update Renamed Renamed
    | Apply Renamed Renamed
    | PartialApply Renamed Renamed
    | Ident Renamed
    | Fetch Renamed
    | Base
    | Heap HeapPointer
    | Tag Renamed NodeType Int [Rhs]
    | VectorTag [Rhs]
    deriving (Eq,Ord,Show)

-- A set of possible rhs values
newtype Rhs = Rhs [RhsValue] deriving (Eq,Ord,Show)
singleton :: RhsValue -> Rhs
singleton value = Rhs [value]

type Equations = Map.Map Lhs Rhs


showEquations :: Equations -> String
showEquations eqs
    = unlines [ show lhs ++ " = " ++ show rhs
              | (lhs, rhs) <- Map.toList eqs ]



instance Monoid Rhs where
    mempty = Rhs []
    mappend (Rhs a) (Rhs b)
        = Rhs (worker a b)
        where worker [] lst = lst
              worker lst [] = lst
              worker (Tag tag1 nt1 missing1 args1:xs) (Tag tag2 nt2 missing2 args2:ys)
                  = case tag1 `compare` tag2 of
                      LT -> Tag tag1 nt1 missing1 args1 : worker xs (Tag tag2 nt2 missing2 args2:ys)
                      GT -> Tag tag2 nt2 missing2 args2 : worker (Tag tag1 nt1 missing1 args1:xs) ys
                      EQ -> Tag tag1 nt1 (min missing1 missing2) (zipJoin args1 args2):worker xs ys
              worker (VectorTag v1:xs) (VectorTag v2:ys)
                  = VectorTag (zipJoin v1 v2) : worker xs ys
{-              worker (y@Tag{}:ys) (x:xs)
                  = x:worker (y:ys) xs
              worker (y:ys) (x@Tag{}:xs)
                  = y:worker ys (x:xs)-}
              worker (y:ys) (x:xs)
                  = case y `compare` x of
                      LT -> y:worker ys (x:xs)
                      GT -> x:worker (y:ys) xs
                      EQ -> x:worker ys xs

isSubsetOf :: Rhs -> Rhs -> Bool
Rhs lRhs `isSubsetOf` Rhs rRhs
    = worker lRhs rRhs
    where worker [] y  = True
          worker x [] = False
          worker (x@(Tag tag1 _ _ args1):xs) (y@(Tag tag2 _ _ args2):ys)
              = case tag1 `compare` tag2 of
                  LT -> False
                  GT -> worker (x:xs) ys
                  EQ -> and (zipWith isSubsetOf args1 args2) && worker xs ys
          worker (x:xs) (y:ys)
              = case x `compare` y of
                  LT -> False
                  GT -> worker (x:xs) ys
                  EQ -> worker xs ys

zipJoin :: Monoid a => [a] -> [a] -> [a]
zipJoin [] []         = []
zipJoin [] lst        = zipWith mappend (repeat mempty) lst
zipJoin lst []        = zipWith mappend lst (repeat mempty)
zipJoin (x:xs) (y:ys) = mappend x y : zipJoin xs ys


type GenReader = Map.Map Renamed [Renamed]
type GenM a = RWS GenReader (Endo Equations) Int a

applications :: Renamed
applications = Builtin "applications"

updates :: Renamed
updates = Builtin "updates"

mkEnvironment :: Grin -> Equations
mkEnvironment grin
    = case execRWS (setupEnvGrin grin) reader 0 of
         (st, eqsEndo) -> appEndo eqsEndo Map.empty
    where reader = Map.fromList [ (funcDefName func, funcDefArgs func) | func <- grinFunctions grin ]

setupEnvGrin :: Grin -> GenM ()
setupEnvGrin grin
    = do forM_ (grinCAFs grin) $ \caf ->
           do hp <- store =<< processVal (cafValue caf)
              addEquation (VarEntry (cafName caf)) (singleton $ Heap hp)
         forM_ (grinFunctions grin) $ \function ->
           do rhs <- setupEnv (funcDefBody function)
              addEquation (VarEntry (funcDefName function)) rhs

-- FIXME: Put these in order.
baseBuiltins, vectorBuiltins, unsupportedBuiltins :: [CompactString]
baseBuiltins        = ["<#",">#","<=#",">=#","-#","+#","*#","narrow32Int#"
                      ,"uncheckedIShiftRA#","and#","==#", "remInt#", "noDuplicate#"
                      ,"narrow8Word#", "writeInt8OffAddr#", "writeWord8OffAddr#", "writeWord64OffAddr#"
                      ,"narrow8Int#", "byteArrayContents#","touch#"
                      ,"uncheckedIShiftL#", "negateInt#", "not#"
                      ,"indexCharOffAddr#","minusWord#","geWord#","eqWord#","narrow16Word#"
                      ,"neWord#", "ltWord#", "gtWord#", "remWord#"
                      ,"ord#","chr#","or#","narrow32Word#","uncheckedShiftL#","plusWord#"
                      ,"uncheckedShiftRL#","neChar#","narrow16Int#","timesWord#"
                      ,"writeAddrOffAddr#","writeInt32OffAddr#","quotInt#", "quotWord#"
                      ,"writeDoubleOffAddr#", "writeWordArray#"
                      ,"leWord#","/=#","writeCharArray#","xor#", "realWorld#"
                      ,"waitWrite#", "negateDouble#", "negateFloat#", "logFloat#", "sqrtDouble#", "expDouble#", "**##"
                      ,"sinDouble#", "tanDouble#", "cosDouble#", "asinDouble#", "atanDouble#"
                      ,"acosDouble#", "asinhDouble#", "sinhDouble#", "tanhDouble#", "coshDouble#"
                      ,"<##", "==##", ">##", "<=##", ">=##", "-##", "+##", "*##", "/##"
                      ,"ltFloat#", "eqFloat#", "writeWord8Array#"
                      ,"coerceDoubleToWord", "coerceWordToDouble", "logDouble#", "int2Double#", "double2Int#"
                      ,"int2Float#", "divideFloat#", "timesFloat#", "minusFloat#", "plusFloat#"
                      ,"gtFloat#", "geFloat#", "leFloat#", "sqrtFloat#"
                      ,"writeWideCharOffAddr#" ]
vectorBuiltins      = ["unsafeFreezeByteArray#", "newAlignedPinnedByteArray#"
                      ,"word2Integer#","integer2Int#", "newByteArray#", "newPinnedByteArray#"
                      ,"readInt8OffAddr#","readInt32OffAddr#","readWord64OffAddr#","readWord8OffAddr#"
                      ,"readAddrOffAddr#","readInt32OffAddr#"
                      ,"readWord8Array#", "readWordArray#", "readDoubleOffAddr#", "writeDoubleOffAddr#"
                      ,"mkWeak#", "readCharArray#"
                      ,"readWideCharOffAddr#" ]
unsupportedBuiltins = ["raise#","atomicModifyMutVar#","writeTVar#"
                      ,"raiseIO#","fork#","atomically#"]


setupEnv :: Expression -> GenM Rhs
setupEnv (Store _size val)
    = do hp <- store =<< processVal val
         return $ singleton $ Heap hp
setupEnv (exp :>>= bind :-> rest)
    = do expRhs <- setupEnv exp
         addEquation (VarEntry bind) expRhs
         setupEnv rest
setupEnv (exp :>> rest)
    = do setupEnv exp
         setupEnv rest
setupEnv (Unit val)
    = processVal val
setupEnv (Case val alts)
    = do let valRhs = singleton $ Ident val
         rets <- forM alts $ \(l :> alt) ->
                   case l of
                     Node tag nt missing args
                       -> do forM_ (zip [0..] args) $ \(n,arg) ->
                               addEquation (VarEntry arg) (singleton $ Extract val (tag, nt, missing) n)
                             setupEnv alt
                     Vector args -> do forM_ (zip [0..] args) $ \(n,arg) ->
                                         addEquation (VarEntry arg) (singleton $ ExtractVector val n)
                                       setupEnv alt
                     Lit{}          -> setupEnv alt
                     Variable v     -> do addEquation (VarEntry v) valRhs
                                          setupEnv alt
                     _              -> error $ "setupEnv: Invalid case: " ++ show l
         return $ mconcat rets
setupEnv (Application External{} args)
    = return $ singleton (VectorTag [singleton Base, singleton Base])

setupEnv (Application (Builtin "eval") [arg])
  = do return $ singleton (Eval arg)
setupEnv (Application (Builtin "apply") [arg1, arg2])
  = do addEquation (VarEntry applications) (singleton $ PartialApply arg1 arg2)
       return $ singleton (Apply arg1 arg2)
--setupEnv (Application (Builtin "update") [ptr,val])
--    = do addEquation (VarEntry updates) (singleton $ Update ptr val)
--           return mempty
setupEnv (Grin.Update size ptr val)
    = do addEquation (VarEntry updates) (singleton $ Update ptr val)
         return mempty
setupEnv (Application (Builtin "caseUpdate") [ptr, val])
    = do addEquation (VarEntry updates) (singleton $ Update ptr val)
         return mempty
setupEnv (Application (Builtin "newMutVar") [val, realWorld])
    = do hp <- store =<< processVal (Variable val)
         return $ singleton $ VectorTag [ singleton Base, singleton $ Heap hp ]
setupEnv (Application (Builtin "updateMutVar") [ptr, val, realWorld])
    = do addEquation (VarEntry updates) (singleton $ Update ptr val)
         return $ singleton Base
setupEnv (Application (Builtin "readMutVar") [val, realWorld])
    = return $ singleton $ VectorTag [ singleton Base, singleton $ Fetch val ]

setupEnv (Application (Builtin fn) args) | fn `elem` baseBuiltins
    = return $ singleton Base
setupEnv (Application (Builtin fn) args) | fn `elem` vectorBuiltins
    = return $ singleton $ VectorTag [singleton Base, singleton Base]
setupEnv (Application (Builtin fn) args) | fn `elem` unsupportedBuiltins
    = return mempty

setupEnv (Application (Builtin fn) args) | Just n <- lookup fn [("decodeDouble_2Int#", 4)
                                                               ,("decodeFloat_Int#", 2)]
    = return $ singleton $ VectorTag (replicate n (singleton Base))

setupEnv (Application (Builtin "makeStablePtr#") [val,realworld])
    = do hp <- store (singleton $ Ident val)
         return $ singleton $ VectorTag [singleton Base, singleton $ Heap hp]
setupEnv (Application (Builtin "deRefStablePtr#") [ptr,realworld])
    = do return $ singleton $ VectorTag [singleton Base, singleton $ Fetch ptr]
setupEnv (Application (Builtin "unblockAsyncExceptions#") [fn, realworld])
    = do return $ singleton $ Apply fn realworld
setupEnv (Application (Builtin "blockAsyncExceptions#") [fn, realworld])
    = do return $ singleton $ Apply fn realworld
setupEnv (Application (Builtin "fetch") [a])
    = return $ singleton $ Fetch a
setupEnv (Application (Builtin "newArray#") [size, elt, realworld])
    = do hp <- store (singleton $ Ident elt)
         return $ singleton $ VectorTag [singleton Base, singleton $ Heap hp]
setupEnv (Application (Builtin "readArray#") [arr, nth, realworld])
    = return $ singleton $ VectorTag [singleton Base, singleton $ Fetch arr]
setupEnv (Application (Builtin "unsafeFreezeArray#") [arr, realworld])
    = return $ singleton $ VectorTag [singleton Base, singleton $ Ident arr]
setupEnv (Application (Builtin "indexArray#") [arr, nth])
    = return $ singleton $ VectorTag [singleton $ Fetch arr ]
setupEnv (Application (Builtin "writeArray#") [arr, nth, elt, realworld])
    = do addEquation (VarEntry updates) (singleton $ Update arr elt)
         return (singleton Base)
setupEnv (Application (Builtin "unreachable") [])
    = return mempty
setupEnv (Application (Builtin builtin) args)
    = error $ "unknown builtin: " ++ show builtin

setupEnv (Application fn args)
    = do funcArgs <- lookupFuncArgs fn
         forM_ (zip funcArgs args) $ \(var, arg) ->
           addEquation (VarEntry var) (singleton $ Ident arg)
         return $ singleton (Ident fn)




processVal :: Value -> GenM Rhs
processVal (Node name nt missing args)
    = do case nt of
           FunctionNode ->
             do funcArgs <- lookupFuncArgs name
                forM_ (zip funcArgs args) $ \(funcArg,arg) ->
                  addEquation (VarEntry funcArg) (singleton $ Ident arg)
                forM_ (takeLast missing $ zip [0..] funcArgs) $ \(index, arg) ->
                  addEquation (VarEntry arg)
                              (singleton $ Extract applications (name, FunctionNode, 0) index)
           ConstructorNode ->
             do return ()
         return $ singleton $ Tag name nt missing (map (singleton . Ident) args)
processVal (Variable var) = return $ singleton $ Ident var
processVal Lit{}          = return $ singleton Base
processVal Hole{}         = return mempty
processVal Empty          = return mempty
processVal (Vector vs)    = return $ singleton $ VectorTag (map (singleton . Ident) vs)

takeLast :: Int -> [a] -> [a]
takeLast size lst = reverse $ take size $ reverse lst

store :: Rhs -> GenM Int
store rhs
  = do u <- get
       put $ u+1
       addEquation (HeapEntry u) rhs
       return u

addEquation :: Lhs -> Rhs -> GenM ()
addEquation lhs rhs
    = tell $ Endo $ Map.insertWith mappend lhs rhs


lookupFuncArgs :: Renamed -> GenM [Renamed]
lookupFuncArgs func
    = asks $ \funcs ->
      case Map.lookup func funcs of
        Nothing   -> error $ "Couldn't find function: " ++ show func
        Just args -> args


