module Data.Bedrock.Storage.Fixed where

import Data.Bedrock
import Data.Bedrock.Storage.Pluggable

{- Allocate a fixed buffer at program start, bump allocate into it.

gc_init =
    pagesize <- sysconf _SC_PAGESIZE;
    mem <- @ccall mmap(null, 1024*pagesize, PROT_READ|PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS , -1, 0);
    writeGlobal "hp" mem

gc_allocate n =
    return success

-}

fixedGC :: GC StorageManager
fixedGC = do
    initName <- newName "init"
    beginName <- newName "begin"
    endName <- newName "end"
    markName <- newName "mark"
    markPtr  <- newVariable "root" NodePtr
    let initFn = Function initName [] [] (Return [])
        beginFn = Function beginName [] [] $
            Panic "FixedGC: Collection not supported!"
        endFn = Function endName [] [] $
            Panic "FixedGC: Collection not supported!"
        markFn = Function markName [markPtr] [NodePtr] $
            Panic "FixedGC: Collection not supported!"
        allocate _size = Unit [LitArg (LiteralInt 1)]
    return StorageManager
        { smInit     = initFn
        , smBegin    = beginFn
        , smEnd      = endFn
        , smMark     = markFn
        , smAllocate = allocate
        }

