{- |
This is the only module that normal users should need to import.

As an example, allocate 1GB of memory, zero it, and crash:

@
import System.Posix.DynamicLinker
import Foreign.Ptr
import Foreign.LibFFI

main = do
    malloc <- dlsym Default \"malloc\"
    memset <- dlsym Default \"memset\"
    p <- callFFI malloc (retPtr retVoid) [argCSize (2^30)]
    callFFI memset (retPtr retVoid) [argPtr p, argCInt 0, argCSize (2^30)]
    callFFI memset (retPtr retVoid) [argPtr nullPtr, argCInt 0, argCSize 1]
@
-}
module Foreign.LibFFI
    (Arg
    ,RetType
    ,callFFI
    ,withRetType
    ,module Foreign.LibFFI.Types
    ) where

import Foreign.LibFFI.Base
import Foreign.LibFFI.Types
