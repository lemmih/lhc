{-# LANGUAGE OverloadedStrings #-}
module Grin.MinusMinus.Convert
    ( convert
    ) where


import Grin.MinusMinus.Types

import qualified Grin.Stage3.Types as Stage3

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Reader
import Control.Monad.State


convert :: Stage3.Grin -> Grin
convert stage3Grin
    = case runState (runReaderT (setupEnv stage3Grin $ mapM convertFuncDef (Stage3.grinFunctions stage3Grin)) Map.empty) (Stage3.grinUnique stage3Grin) of
        (fns, newUnique) -> Grin { grinNodes = Stage3.grinNodes stage3Grin
                                 , grinCAFs  = Stage3.grinCAFs stage3Grin
                                 , grinFunctions = fns
                                 , grinEntryPoint = Stage3.grinEntryPoint stage3Grin
                                 , grinUnique = newUnique }

type M = ReaderT (Map.Map Renamed ([Register], [Register])) (State Int)

setupEnv :: Stage3.Grin -> M a -> M a
setupEnv stage3Grin action
    = worker (Stage3.grinFunctions stage3Grin)
    where worker [] = action
          worker (fn:fns) = do outRegs <- replicateM (Stage3.funcDefReturns fn) newVariable
                               local (Map.insert (Stage3.funcDefName fn) (Stage3.funcDefArgs fn, outRegs)) $ worker fns

convertFuncDef :: Stage3.FuncDef -> M FuncDef
convertFuncDef oldFunc
    = do (inRegs, outRegs) <- getFunctionRegisters (Stage3.funcDefName oldFunc)
         body <- convertExpression outRegs (Stage3.funcDefBody oldFunc)
         return $ FuncDef { funcDefName = Stage3.funcDefName oldFunc
                          , funcDefReturnRegisters = outRegs
                          , funcDefArgRegisters = inRegs
                          , funcDefBody = body }

convertExpression :: [Register] -> Stage3.Expression -> M Body
convertExpression outRegisters exp
    = case exp of
        Stage3.Singleton simple
          -> do stmts <- convertSExpression outRegisters simple
                return $ bodyFromStatements stmts Return
        simple Stage3.:>>= binds Stage3.:-> rest
          -> do stmts <- convertSExpression binds simple
                lastBlock <- convertExpression outRegisters rest
                return $ bodyFromStatements stmts lastBlock
        Stage3.Case scrut alts
          -> do alts' <- forM alts $ \(val Stage3.:> simple) -> do stmts <- convertSExpression outRegisters simple
                                                                   return ( val :> bodyFromStatements stmts Return)
                return $ Case scrut alts'

convertSExpression :: [Register] -> Stage3.SimpleExpression -> M [Statement]
convertSExpression outRegisters exp
    = case exp of
        --Stage3.Application (Builtin fn) args | fn `Set.member` ioPrimitives
        --  -> return [ PrimopSet (outRegisters!!1) (Builtin fn) (init args) ]
        Stage3.Application (Builtin fn) args | fn `Set.member` voidPrimitives
          -> return [ Primop fn (init args) ]
        Stage3.Application (Builtin fn) args
          -> case outRegisters of
               [] -> return [ Primop fn args ]
               [x]  -> return [ PrimopSet x fn args ]
               [x,y] -> return [ PrimopSet y fn (init args) ]
        Stage3.Application (External name types) args
          -> return [ CCall (outRegisters!!1) name types (init args) ]
        Stage3.Application fn args -> do (fnInRegs, fnOutRegs) <- getFunctionRegisters fn
                                         return (-- [ Push reg | reg <- fnInRegs ] ++
                                                 [ Move dst src | (dst,src) <- zip fnInRegs args ] ++
                                                 [ Call fn ] ++
                                                 [ Move dst src | (dst,src) <- zip outRegisters fnOutRegs ]
                                                 -- [ Pop reg | reg <- reverse fnInRegs ]
                                                )
        Stage3.Fetch idx ptr   -> return [ Fetch (head outRegisters) idx ptr ]
        Stage3.Store size vars -> return [ Store (head outRegisters) size vars ]
        Stage3.StoreHole size  -> return [ StoreHole (head outRegisters) size ]
        Stage3.Unit vars       -> return [ Move dst src | (dst, src) <- zip outRegisters vars ]
        Stage3.Constant val    -> return [ Constant (head outRegisters) val ]
    where firstOutRegister = case outRegisters of
                               (x:_) -> x
                               _     -> error $ "Grin.MinusMinus.Convert.convertSExpression: Expected at least a single output register: " ++ show exp

voidPrimitives = Set.fromList [ "writeInt8OffAddr#", "writeWideCharOffAddr#", "writeAddrOffAddr#", "writeWord64OffAddr#"
                              , "touch#", "updateMutVar" ]

getFunctionRegisters :: Renamed -> M ([Register], [Register])
getFunctionRegisters fn
    = asks $ Map.findWithDefault errMsg fn
    where errMsg = error $ "Grin.MinusMinus.Convert.getFunctionRegisters: Unknown function: " ++ show fn


bodyFromStatements :: [Statement] -> Body -> Body
bodyFromStatements stmts end
    = worker stmts
    where worker [] = end
          worker (x:xs) = x :>> worker xs

newVariable :: M Renamed
newVariable
    = do u <- get
         put $! u+1
         return $ Anonymous u

