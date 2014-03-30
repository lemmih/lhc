{-# LANGUAGE GADTs, ExistentialQuantification, NoImplicitPrelude, Rank2Types #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module ExplicitIO where

import Prelude (Show(..), Monad(..), undefined, String, (++) )

import qualified Data.IORef as Prelude
import qualified Prelude
import qualified System.IO.Unsafe as Prelude


{-# NOINLINE threadList #-}
threadList :: Prelude.IORef [AnyIOPrim]
threadList = Prelude.unsafePerformIO (Prelude.newIORef [])



pushThread :: IOPrim a -> Prelude.IO ()
pushThread prim = Prelude.modifyIORef threadList (\lst -> AnyIOPrim prim : lst)

popThread :: Prelude.IO AnyIOPrim
popThread = do
  threads <- Prelude.readIORef threadList
  case threads of
    [] -> Prelude.error "No more threads."
    (x:xs) -> do
      Prelude.writeIORef threadList xs
      return x

data Bool = True | False
  deriving (Show)



execIO :: IO a -> Prelude.IO a
execIO io = runIOPrim (runIO io Unit)


newtype IO a = IO { runIO :: forall r. (a -> IOPrim r) -> IOPrim r }

instance Monad IO where
  return a = IO (\cont -> cont a)
  (>>=) f g = IO (\cont -> runIO f (\a -> runIO (g a) cont))


terminate :: Bool -> IO ()
terminate False = return ()
terminate True  = IO (\_ -> Terminate)

data AnyIOPrim = forall r. AnyIOPrim (IOPrim r)

data IOPrim a where
  Unit       :: a -> IOPrim a
  Terminate  :: IOPrim a
  NewIORef   :: a -> IOPrim (IORef a)
  ReadIORef  :: IORef a -> IOPrim a
  WriteIORef :: IORef a -> a -> IOPrim ()
  Bind       :: IOPrim a -> (a -> IOPrim b) -> IOPrim b
  --CFFI :: String -> [PrimType] -> IO PrimType
  -- BlockingFFI
  -- NonBlockingFFI

  -- Basic I/O
  PutStrLn   :: String -> IOPrim ()

--  -- libuv
  ASyncSpawn :: forall a. IOPrim a -> IOPrim ()
  Yield      :: IOPrim a

  -- Exception handling
  ThrowIO :: SomeException -> IOPrim a
  --ThrowTo :: ThreadId -> SomeException -> IOPrim a
  Catch :: (SomeException -> IOPrim a) -> IOPrim a -> IOPrim a

runIOPrim :: IOPrim a -> Prelude.IO a
runIOPrim prim =
  case prim of
    Unit a           -> return a
    Terminate        -> Prelude.error "Terminate"
    NewIORef a       -> Prelude.newIORef a
    ReadIORef ref    -> Prelude.readIORef ref
    WriteIORef ref a -> Prelude.writeIORef ref a
    Bind f g         -> do
      a <- runIOPrim f
      runIOPrim (g a)

    PutStrLn str -> Prelude.putStrLn str

    ASyncSpawn prim -> do
      Prelude.putStrLn "Spawn"
      pushThread prim
    Yield -> do
      thread <- popThread
      Prelude.putStrLn "Yield"
      case thread of
        AnyIOPrim prim -> runIOPrim prim >> runIOPrim Yield




putStrLn :: String -> IO ()
putStrLn str = IO (\cont -> PutStrLn str `Bind` cont)

-- Assume IORefs
type IORef a = Prelude.IORef a

newIORef :: a -> IO (IORef a)
newIORef a = IO (\cont -> NewIORef a `Bind` cont)

readIORef :: IORef a -> IO a
readIORef ref = IO (\cont -> ReadIORef ref `Bind` cont)

writeIORef :: IORef a -> a -> IO ()
writeIORef ref a = IO (\cont -> WriteIORef ref a `Bind` (\_ -> cont ()))


data MVar a = MVar (IORef (MVarContent a))
data MVarContent a =
  MVarReaders [a -> AnyIOPrim] |
  MVarWriters a [(a,AnyIOPrim)]

newEmptyMVar :: IO (MVar a)
newEmptyMVar = IO (\cont ->
  NewIORef (MVarReaders []) `Bind` \ref ->
  cont (MVar ref)
  )

takeMVar :: MVar a -> IO a
takeMVar (MVar ref) = IO (\cont ->
  ReadIORef ref `Bind` \content ->
  case content of
    MVarReaders lst ->
      let lst' = (\a -> AnyIOPrim (cont a)) : lst in
      WriteIORef ref (MVarReaders lst') `Bind` \_ ->
      Yield
    MVarWriters a [] ->
      WriteIORef ref (MVarReaders []) `Bind` \_ ->
      cont a
    MVarWriters a ((a',x):xs) ->
      WriteIORef ref (MVarWriters a' xs) `Bind` \_ ->
      cont a
  )

putMVar :: MVar a -> a -> IO ()
putMVar (MVar ref) a = IO (\cont ->
  ReadIORef ref `Bind` \content ->
  case content of
    MVarReaders [] ->
      WriteIORef ref (MVarWriters a []) `Bind` \_ ->
      cont ()
    MVarReaders (x:xs) ->
      WriteIORef ref (MVarReaders xs) `Bind` \_ ->
      case x a of
        AnyIOPrim t ->
          ASyncSpawn t `Bind` cont
    MVarWriters orig lst ->
      WriteIORef ref (MVarWriters orig ((a, AnyIOPrim (cont ())) : lst)) `Bind` \_ ->
      Yield
  )


spawn :: IO () -> IO ()
spawn io = IO (\cont -> ASyncSpawn ((runIO io Unit)) `Bind` cont)




testProgram :: IO ()
testProgram = do
  ref <- newEmptyMVar
  spawn (putMVar ref True)
  val <- takeMVar ref
  putStrLn ("Boolean: " ++ show val)


testYield :: IO Bool
testYield = IO (\cont -> Yield)

testReturn :: IO Bool
testReturn = return True


{-

fn = do
  v <- store ...
  a <- fnA v
  b <- fnB v
  return a+b

fn = \cont ->
  store ... (\v ->
    fnA v (\a ->
      fnB v (\b ->
        cont (a+b)
      )
    )
  )

fn = \cont ->
  store_fn ... cont

store_fn ... cont =
  if no heap
    mark_root cont
    do_gc

  let v = ...
  fn_fnA v cont

fn_fnA v cont =
  let a = ...
  fn_fnB v a cont

fn_fnB v a cont =
  let b = ...
  cont (a+b)

gc_alloc :: (Ptr -> IO a) -> Int -> IO a



foreign import "prim cont" unsafePerformIO :: IO a -> a

mkContinuation :: ((a -> IOPrim r) -> IOPrim r) -> a

unsafePerformIO :: IO a -> a
unsafePerformIO io = mkContinuation $ \cont ->
  runIO io cont



-}


unsafeCoerce :: a -> b
unsafeCoerce = undefined

data Any

data SomeException = forall x. Show x => SomeException x


throw :: SomeException -> a
throw = undefined

-- Primitive.
--foreign import ccall mkContinuation :: ((a -> IOPrim r) -> IOPrim r) -> a
mkContinuation :: ((a -> IOPrim r) -> IOPrim r) -> a
mkContinuation = undefined


unsafePerformIO :: IO a -> a
unsafePerformIO io = mkContinuation (\cont -> runIO io cont)

{-
ioFromPure cont = runIO getLine cont
-}
data Addr = Addr
gc_alloc :: (Addr -> IOPrim a) -> IOPrim a
gc_alloc cont = undefined






