module Data.Bedrock.Storage.Cached where

import Data.Bedrock
import Data.Bedrock.Storage.Pluggable
import qualified LLVM.AST            as LLVM (Type (..))
import           LLVM.AST.Type          as LLVM

cachedSpaceGC :: GC StorageManager
cachedSpaceGC = do
    pushForeign $ Foreign
        { foreignName = "_lhc_cached_init"
        , foreignReturn = ptr i64
        , foreignArguments = [] }
    pushForeign $ Foreign
        { foreignName = "_lhc_cached_begin"
        , foreignReturn = LLVM.VoidType
        , foreignArguments = [] }
    pushForeign $ Foreign
        { foreignName = "_lhc_cached_end"
        , foreignReturn = ptr i64
        , foreignArguments = [] }
    pushForeign $ Foreign
        { foreignName = "_lhc_cached_allocate"
        , foreignReturn = i64
        , foreignArguments = [ptr i64, i64] }
    pushForeign $ Foreign
        { foreignName = "_lhc_cached_mark"
        , foreignReturn = ptr i64
        , foreignArguments = [ptr i64] }
    pushForeign $ Foreign
        { foreignName = "_lhc_cached_mark_frame"
        , foreignReturn = ptr i64
        , foreignArguments = [ptr i64] }


    initName <- newName "cached_gc_init"
    rawPtr <- newVariable "ptr" (Primitive (ptr i64))
    hpVar <- newVariable "hp" NodePtr
    beginName <- newName "cached_gc_begin"
    endName <- newName "cached_gc_end"
    markName <- newName "cached_gc_mark"
    markFrameName <- newName "cached_gc_mark_frame"
    markPtr  <- newVariable "root" NodePtr
    markRet  <- newVariable "ret" NodePtr
    markFrame  <- newVariable "frame" FramePtr
    let initFn = Function initName [] [] [] $
            Bind [rawPtr] (CCall "_lhc_cached_init" []) $
            Bind [hpVar] (TypeCast rawPtr) $
            Bind [] (WriteGlobal "hp" hpVar) $
            Return []
        beginFn = Function beginName [] [] [] $
            Bind [] (CCall "_lhc_cached_begin" []) $ Return []
        endFn = Function endName [] [] [NodePtr] $
            Bind [rawPtr] (CCall "_lhc_cached_end" []) $
            Bind [hpVar] (TypeCast rawPtr) $
            Return [hpVar]
        markFn = Function markName [] [markPtr] [NodePtr] $
            Bind [markRet] (CCall "_lhc_cached_mark" [markPtr]) $ Return [markRet]
        markFrameFn = Function markFrameName [] [markFrame] [FramePtr] $
            Bind [markRet] (CCall "_lhc_cached_mark_frame" [markFrame]) $ Return [markRet]
        allocate hp size = CCall "_lhc_cached_allocate" [hp, size]
    return StorageManager
        { smInit     = initFn
        , smBegin    = beginFn
        , smEnd      = endFn
        , smMark     = markFn
        , smMarkFrame = markFrameFn
        , smAllocate = allocate
        }
