module Data.Bedrock.Storage.SemiSpace where

import Data.Bedrock
import Data.Bedrock.Storage.Pluggable
import qualified LLVM.AST            as LLVM (Type (..))
import           LLVM.AST.Type          as LLVM

semiSpaceGC :: GC StorageManager
semiSpaceGC = do
    pushForeign $ Foreign
        { foreignName = "_lhc_semi_init"
        , foreignReturn = ptr i64
        , foreignArguments = [] }
    pushForeign $ Foreign
        { foreignName = "_lhc_semi_begin"
        , foreignReturn = LLVM.VoidType
        , foreignArguments = [] }
    pushForeign $ Foreign
        { foreignName = "_lhc_semi_end"
        , foreignReturn = ptr i64
        , foreignArguments = [] }
    pushForeign $ Foreign
        { foreignName = "_lhc_semi_allocate"
        , foreignReturn = i64
        , foreignArguments = [ptr i64, i64] }
    pushForeign $ Foreign
        { foreignName = "_lhc_semi_mark"
        , foreignReturn = ptr i64
        , foreignArguments = [ptr i64] }
    pushForeign $ Foreign
        { foreignName = "_lhc_semi_mark_frame"
        , foreignReturn = ptr i64
        , foreignArguments = [ptr i64] }


    initName <- newName "semi_gc_init"
    rawPtr <- newVariable "ptr" (Primitive (ptr i64))
    hpVar <- newVariable "hp" NodePtr
    beginName <- newName "semi_gc_begin"
    endName <- newName "semi_gc_end"
    markName <- newName "semi_gc_mark"
    markFrameName <- newName "semi_gc_mark_frame"
    markPtr  <- newVariable "root" NodePtr
    markRet  <- newVariable "ret" NodePtr
    markFrame  <- newVariable "frame" FramePtr
    let initFn = Function initName [] [] [] $
            Bind [rawPtr] (CCall "_lhc_semi_init" []) $
            Bind [hpVar] (TypeCast rawPtr) $
            Bind [] (WriteGlobal "hp" hpVar) $
            Return []
        beginFn = Function beginName [] [] [] $
            Bind [] (CCall "_lhc_semi_begin" []) $ Return []
        endFn = Function endName [] [] [NodePtr] $
            Bind [rawPtr] (CCall "_lhc_semi_end" []) $
            Bind [hpVar] (TypeCast rawPtr) $
            Return [hpVar]
        markFn = Function markName [] [markPtr] [NodePtr] $
            Bind [markRet] (CCall "_lhc_semi_mark" [markPtr]) $ Return [markRet]
        markFrameFn = Function markFrameName [] [markFrame] [FramePtr] $
            Bind [markRet] (CCall "_lhc_semi_mark_frame" [markFrame]) $ Return [markRet]
        allocate hp size = CCall "_lhc_semi_allocate" [hp, size]
    return StorageManager
        { smInit     = initFn
        , smBegin    = beginFn
        , smEnd      = endFn
        , smMark     = markFn
        , smMarkFrame = markFrameFn
        , smAllocate = allocate
        }
