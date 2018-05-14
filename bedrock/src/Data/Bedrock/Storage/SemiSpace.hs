module Data.Bedrock.Storage.SemiSpace where

import Data.Bedrock
import Data.Bedrock.Storage.Pluggable

semiSpaceGC :: GC StorageManager
semiSpaceGC = do
    pushForeign $ Foreign
        { foreignName = "_lhc_semi_init"
        , foreignReturn = CPointer I64
        , foreignArguments = [] }
    pushForeign $ Foreign
        { foreignName = "_lhc_semi_begin"
        , foreignReturn = CVoid
        , foreignArguments = [] }
    pushForeign $ Foreign
        { foreignName = "_lhc_semi_end"
        , foreignReturn = CPointer I64
        , foreignArguments = [] }
    pushForeign $ Foreign
        { foreignName = "_lhc_semi_allocate"
        , foreignReturn = I64
        , foreignArguments = [CPointer I64, I64] }
    pushForeign $ Foreign
        { foreignName = "_lhc_semi_mark"
        , foreignReturn = CPointer I64
        , foreignArguments = [CPointer I64] }
    pushForeign $ Foreign
        { foreignName = "_lhc_semi_mark_frame"
        , foreignReturn = CPointer I64
        , foreignArguments = [CPointer I64] }


    initName <- newName "semi_gc_init"
    rawPtr <- newVariable "ptr" (Primitive (CPointer I64))
    hp <- newVariable "hp" NodePtr
    beginName <- newName "semi_gc_begin"
    endName <- newName "semi_gc_end"
    markName <- newName "semi_gc_mark"
    markFrameName <- newName "semi_gc_mark_frame"
    markPtr  <- newVariable "root" NodePtr
    markRet  <- newVariable "ret" NodePtr
    markFrame  <- newVariable "frame" FramePtr
    let initFn = Function initName [] [] [] $
            Bind [rawPtr] (CCall "_lhc_semi_init" []) $
            Bind [hp] (TypeCast rawPtr) $
            Bind [] (WriteGlobal "hp" hp) $
            Return []
        beginFn = Function beginName [] [] [] $
            Bind [] (CCall "_lhc_semi_begin" []) $ Return []
        endFn = Function endName [] [] [NodePtr] $
            Bind [rawPtr] (CCall "_lhc_semi_end" []) $
            Bind [hp] (TypeCast rawPtr) $
            Return [hp]
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
