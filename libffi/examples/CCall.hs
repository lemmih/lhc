{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative hiding (Alternative(..), many)
import Control.Monad.State.Strict
import Control.Exception hiding (try)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Int
import Data.Word
import Data.Char
import Text.ParserCombinators.Parsec
import System.IO
import System.Posix.DynamicLinker
import Foreign.C.Types
import Foreign.Ptr
import Foreign.LibFFI
import Prelude hiding (catch)

instance Applicative (GenParser tok st) where
    pure    = return
    (<*>)   = ap

pRead   :: Read a => CharParser st a
pRead = do
    s <- getInput
    case reads s of
        []          -> fail "no reads result"
        [(a, s')]   -> setInput s' >> return a
        _           -> fail "ambiguous reads result"

data Val    = I CInt
            | IL CLong
            | I8 Int8
            | I16 Int16
            | I32 Int32
            | I64 Int64

            | U CUInt
            | UL CULong
            | U8 Word8
            | U16 Word16
            | U32 Word32
            | U64 Word64

            | Z CSize

            | F CFloat
            | D CDouble

            | P (Ptr ())
            | S String
            deriving (Eq, Show)

valToArg val = case val of
                I x     -> argCInt x
                IL x    -> argCLong x
                I8 x    -> argInt8 x
                I16 x   -> argInt16 x
                I32 x   -> argInt32 x
                I64 x   -> argInt64 x
                U x     -> argCUInt x
                UL x    -> argCULong x
                U8 x    -> argWord8 x
                U16 x   -> argWord16 x
                U32 x   -> argWord32 x
                U64 x   -> argWord64 x
                Z x     -> argCSize x
                F x     -> argCFloat x
                D x     -> argCDouble x
                P x     -> argPtr x
                S x     -> argString x

pIdent :: CharParser st String
pIdent = liftM2 (:) (char '_' <|> letter) (many $ char '_' <|> alphaNum) <?> "identifier"

pArg :: CharParser (Map String Val) Val
pArg = liftM S pRead
    <|> do
        i <- pRead :: CharParser st Integer
        t <- many alphaNum
        case t of
            ""      -> return $ I $ fromIntegral i
            "i"     -> return $ I $ fromIntegral i
            "l"     -> return $ IL $ fromIntegral i
            "i8"    -> return $ I8 $ fromIntegral i
            "i16"   -> return $ I16 $ fromIntegral i
            "i32"   -> return $ I32 $ fromIntegral i
            "i64"   -> return $ I64 $ fromIntegral i
            "u"     -> return $ U $ fromIntegral i
            "ul"    -> return $ UL $ fromIntegral i
            "u8"    -> return $ U8 $ fromIntegral i
            "u16"   -> return $ U16 $ fromIntegral i
            "u32"   -> return $ U32 $ fromIntegral i
            "u64"   -> return $ U64 $ fromIntegral i
            "p"     -> return $ P $ plusPtr nullPtr $ fromIntegral i
            "z"     -> return $ Z $ fromIntegral i
            _       -> fail "invalid type"
    <|> do
        x <- pRead :: CharParser st Double
        t <- many alphaNum
        case t of
            ""      -> return $ D $ realToFrac x
            "s"     -> return $ F $ realToFrac x
            _       -> fail "invalid type"
    <|> do
        ident <- pIdent
        env <- getState
        case Map.lookup ident env of
            Nothing -> fail "no such identifier"
            Just v  -> return v

pRet :: CharParser st (Maybe (RetType Val))
pRet = do
    t <- many1 alphaNum
    case t of
        "v"     -> return Nothing
        "i"     -> return $ Just $ fmap I   retCInt
        "l"     -> return $ Just $ fmap IL  retCLong
        "i8"    -> return $ Just $ fmap I8  retInt8
        "i16"   -> return $ Just $ fmap I16 retInt16
        "i32"   -> return $ Just $ fmap I32 retInt32
        "i64"   -> return $ Just $ fmap I64 retInt64
        "u"     -> return $ Just $ fmap U   retCUInt
        "ul"    -> return $ Just $ fmap UL  retCULong
        "u8"    -> return $ Just $ fmap U8  retWord8
        "u16"   -> return $ Just $ fmap U16 retWord16
        "u32"   -> return $ Just $ fmap U32 retWord32
        "u64"   -> return $ Just $ fmap U64 retWord64
        "p"     -> return $ Just $ fmap P   (retPtr retVoid)
        "z"     -> return $ Just $ fmap Z   retCSize
        "f"     -> return $ Just $ fmap F   retCFloat
        "d"     -> return $ Just $ fmap D   retCDouble
        "s"     -> return $ Just $ fmap S   retString
        _       -> fail "invalid type"

pCall :: CharParser (Map String Val) ((String -> IO (FunPtr a)) -> IO (Maybe (String, Val)))
pCall = do
    mbAssign <- optionMaybe $ try $ pIdent <* (spaces >> char '=' >> spaces)
    mbRet <- pRet
    space
    sym <- pIdent
    vals <- many (space >> pArg)
    let call f retType = return $ \load -> load sym >>= \fp -> f <$> callFFI fp retType (map valToArg vals)
    case (mbAssign, mbRet) of
        (Just ident, Just retType)  -> call (Just . (,) ident) retType
        (Nothing   , Just retType)  -> call (Just . (,) "it" ) retType
        (Nothing   , Nothing     )  -> call id                 (const Nothing <$> retVoid)
        (Just ident, Nothing)       -> fail "cannot assign void"

repl env = do
    putStr "> "
    hFlush stdout
    s <- getLine `catch` (\(e :: IOException) -> return ":q")
    case s of
        ":q" -> return ()
        ":l" -> do
            forM_ (Map.toList env) $ \(ident, val) -> putStrLn $ ident ++ " = " ++ show val
            repl env
        _ -> do
            case words s of
                [ident] -> do
                    case Map.lookup ident env of
                        Nothing -> putStrLn ("No such identifier: " ++ show ident)
                        Just val -> print val
                    repl env
                _ -> case runParser pCall env "repl" s of
                        Left err    -> print err >> repl env
                        Right call  -> do
                            mbAssign <- call (dlsym Default)
                                            `catch` (\(e :: IOException) -> print e >> return Nothing)
                            repl $ maybe id (uncurry Map.insert) mbAssign env

main = repl Map.empty
