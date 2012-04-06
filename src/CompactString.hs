{-# LANGUAGE BangPatterns #-}
module CompactString
    ( CompactString
    , fromString
    , fromByteString
    , fromLazyByteString
    , append
    , qualToCompact
    ) where

import Data.IORef
import qualified Data.Trie as Trie

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import System.IO.Unsafe

import Data.Digest.CRC32
import Data.Binary
import Data.Char
import Text.PrettyPrint.ANSI.Leijen
import Control.Monad (liftM2)
import qualified Data.String as String

instance String.IsString CompactString where
    fromString = fromString -- Surprise, surprise, this is not a loop.

data CompactString = CompactString { csString :: S.ByteString
                                   , csHash   :: Word32 }

instance Binary CompactString where
    put cs = put (csString cs) >> put (csHash cs)
    get = liftM2 CompactString get get

instance Pretty CompactString where
    pretty = pretty . S.unpack . csString

instance Show CompactString where
    show = show . csString

instance Read CompactString where
    readsPrec n str = [ (fromString n, rest) | (n,rest) <- readsPrec n str]

instance Eq CompactString where
    cs1 == cs2 = if csHash cs1 == csHash cs2
                 then csString cs1 == csString cs2
                 else False

instance Ord CompactString where
    cs1 `compare` cs2
      = case csHash cs1 `compare` csHash cs2 of
          GT -> GT
          LT -> LT
          EQ -> csString cs1 `compare` csString cs2

{-# NOINLINE globalTable #-}
globalTable :: IORef (Trie.Trie CompactString)
globalTable = unsafePerformIO (newIORef Trie.empty)

append :: CompactString -> CompactString -> CompactString
append !a !b
    = fromByteString (csString a `S.append` csString b)

fromString :: String -> CompactString
fromString = fromByteString . S.pack

fromLazyByteString :: L.ByteString -> CompactString
fromLazyByteString = fromByteString . S.concat . L.toChunks

fromByteString :: S.ByteString -> CompactString
fromByteString str
    = unsafePerformIO $
      atomicModifyIORef globalTable $ \trie ->
        case Trie.lookup str trie of
          Nothing -> let cs = CompactString { csString = str
                                            , csHash   = crc32 str }
                     in (Trie.insert str cs trie, cs)
          Just cs -> (trie,cs)

qualToCompact :: (L.ByteString, L.ByteString, L.ByteString) -> CompactString
qualToCompact (pkg,mod,ident)
    | L.null pkg && L.null mod = fromLazyByteString $ zDecodeByteString ident
    | otherwise                = fromLazyByteString (L.concat [zDecodeByteString pkg
                                                              , L.pack ":"
                                                              , zDecodeByteString mod
                                                              , L.pack "."
                                                              , zDecodeByteString ident])

-- Stolen from GHC. (C) The University of Glasgow.
zDecodeByteString :: L.ByteString -> L.ByteString
zDecodeByteString = L.pack . zDecodeString . L.unpack

zDecodeString :: String -> String
zDecodeString [] = []
zDecodeString ('Z' : d : rest)
  | isDigit d = decode_tuple   d rest
  | otherwise = decode_upper   d : zDecodeString rest
zDecodeString ('z' : d : rest)
  | isDigit d = decode_num_esc d rest
  | otherwise = decode_lower   d : zDecodeString rest
zDecodeString (c   : rest) = c : zDecodeString rest

decode_upper, decode_lower :: Char -> Char

decode_upper 'L' = '('
decode_upper 'R' = ')'
decode_upper 'M' = '['
decode_upper 'N' = ']'
decode_upper 'C' = ':'
decode_upper 'Z' = 'Z'
decode_upper ch  = {-pprTrace "decode_upper" (char ch)-} ch

decode_lower 'z' = 'z'
decode_lower 'a' = '&'
decode_lower 'b' = '|'
decode_lower 'c' = '^'
decode_lower 'd' = '$'
decode_lower 'e' = '='
decode_lower 'g' = '>'
decode_lower 'h' = '#'
decode_lower 'i' = '.'
decode_lower 'l' = '<'
decode_lower 'm' = '-'
decode_lower 'n' = '!'
decode_lower 'p' = '+'
decode_lower 'q' = '\''
decode_lower 'r' = '\\'
decode_lower 's' = '/'
decode_lower 't' = '*'
decode_lower 'u' = '_'
decode_lower 'v' = '%'
decode_lower ch  = {-pprTrace "decode_lower" (char ch)-} ch

-- Characters not having a specific code are coded as z224U (in hex)
decode_num_esc :: Char -> String -> String
decode_num_esc d rest
  = go (digitToInt d) rest
  where
    go n (c : rest) | isHexDigit c = go (16*n + digitToInt c) rest
    go n ('U' : rest)           = chr n : zDecodeString rest
    go n other = error ("decode_num_esc: " ++ show n ++  ' ':other)

decode_tuple :: Char -> String -> String
decode_tuple d rest
  = go (digitToInt d) rest
  where
        -- NB. recurse back to zDecodeString after decoding the tuple, because
        -- the tuple might be embedded in a longer name.
    go n (c : rest) | isDigit c = go (10*n + digitToInt c) rest
    go 0 ('T':rest)     = "()" ++ zDecodeString rest
    go n ('T':rest)     = '(' : replicate (n-1) ',' ++ ")" ++ zDecodeString rest
    go 1 ('H':rest)     = "(# #)" ++ zDecodeString rest
    go n ('H':rest)     = '(' : '#' : replicate (n-1) ',' ++ "#)" ++ zDecodeString rest
    go n other = error ("decode_tuple: " ++ show n ++ ' ':other)

