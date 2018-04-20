module Data.Bedrock.Storage.Trace where

import Data.Bedrock
import Data.Bedrock.Storage.Pluggable

{- Allocate a fixed buffer at program start, bump allocate into it.

gc_init =
    pagesize <- sysconf _SC_PAGESIZE;
    mem <- @ccall mmap(null, 1024*pagesize, PROT_READ|PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS , -1, 0);
    writeGlobal "hp" mem

-}

traceGC :: GC StorageManager
traceGC = do
    pushForeign $ Foreign
        { foreignName = "calloc"
        , foreignReturn = CPointer I8
        , foreignArguments = [I32, I32] }
    initName <- newName "trace_gc_init"
    rawPtr <- newVariable "ptr" (Primitive (CPointer I8))
    hp <- newVariable "hp" NodePtr
    wordSize <- newVariable "size" (Primitive I32)
    heapSize <- newVariable "heapSize" (Primitive I32)
    beginName <- newName "trace_gc_begin"
    endName <- newName "trace_gc_end"
    markName <- newName "trace_gc_mark"
    markFrameName <- newName "trace_gc_mark_frame"
    markPtr  <- newVariable "root" NodePtr
    markFrame <- newVariable "frame" (FramePtr 0)

    markBody <- do
      val <- newVariable "ptr" IWord
      retAddr <- newVariable "retAddr" IWord
      return $
        Bind [val] (TypeCast markFrame) $
        Case val (Just $
                    Bind [retAddr] (Load markFrame 0) $
                    Bind [] (InvokeReturn 1 retAddr [markFrame]) $
                    Return [markFrame])
          [ Alternative (LitPat (LiteralInt 0)) (Return [markFrame]) ]


    let initFn = Function initName [] [] [] $
            Bind [wordSize] (Literal (LiteralInt 8)) $
            Bind [heapSize] (Literal (LiteralInt 10240)) $
            Bind [rawPtr] (CCall "calloc" [heapSize, wordSize]) $
            Bind [hp] (TypeCast rawPtr) $
            Bind [] (WriteGlobal "hp" hp) $
            Return []
        beginFn = Function beginName [] [] [] $
            Return []
        endFn = Function endName [] [] [] $
            Return []
        markFn = Function markName [] [markPtr] [NodePtr] $
            --Panic "TraceGC: Collection not supported!"
            Return [markPtr]
        markFrameFn = Function markFrameName [] [markFrame] [FramePtr 0] $
            markBody
        allocate _size = Literal (LiteralInt 0)
    return StorageManager
        { smInit     = initFn
        , smBegin    = beginFn
        , smEnd      = endFn
        , smMark     = markFn
        , smMarkFrame = markFrameFn
        , smAllocate = allocate
        }
