{-# LANGUAGE GADTs, ExistentialQuantification, NoImplicitPrelude, Rank2Types #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash, BangPatterns #-}
module DirectIO where

import Prelude (Show(..), Monad(..), undefined, String, (++), Int, seq, (+), (-) )

import qualified Data.IORef as Prelude
import qualified Prelude
import qualified System.IO.Unsafe as Prelude
import GHC.Prim (unsafeCoerce#)

data ThreadId = ThreadId (IORef ThreadStatus)
data ThreadStatus = ThreadFinished | ThreadRunning (ThreadList -> SomeException -> Prelude.IO ())

throwTo :: ThreadId -> SomeException -> IO ()
throwTo (ThreadId ref) e = do
  status <- readIORef ref
  case status of
    ThreadFinished -> Prelude.error "throwTo: Thread finished."
    ThreadRunning signal ->
      -- IO (\tid ts exc cont -> cheat (signal ts))
      IO (\tid ts exc cont -> cheat (signal (pushThread (\ts' -> cheat (cont ts' ())) ts) e) )

--newtype ThreadList = ThreadList [ThreadList -> Prelude.IO ()]
type Thread = ThreadList -> Prelude.IO ()
data ThreadList
  = Nil
  | OnePlus !Thread !ThreadList
  | TwoPlus !Thread !Thread !ThreadList
  | ThreePlus !Thread !Thread !Thread !ThreadList
  | FourPlus !Thread !Thread !Thread !Thread !ThreadList
  | FivePlus !Thread !Thread !Thread !Thread !Thread !ThreadList

emptyThreadList :: ThreadList
emptyThreadList = Nil

pushThread :: Thread -> ThreadList -> ThreadList
pushThread elt (OnePlus a ts) = TwoPlus elt a ts
pushThread elt (TwoPlus a b ts) = ThreePlus elt a b ts
pushThread elt (ThreePlus a b c ts) = FourPlus elt a b c ts
pushThread elt (FourPlus a b c d ts) = FivePlus elt a b c d ts
pushThread elt lst = OnePlus elt lst
--pushThread elt (ThreadList lst) = ThreadList (elt : lst)

popThread :: ThreadList -> Prelude.IO ()
popThread ts =
  case ts of
    Nil                 -> Prelude.error "No more threads to run."
    OnePlus thread ts'  -> thread ts'
    TwoPlus a b ts'     -> a (OnePlus b ts')
    ThreePlus a b c ts' -> a (TwoPlus b c ts')
    FourPlus a b c d ts' -> a (ThreePlus b c d ts')
    FivePlus a b c d e ts' -> a (FourPlus b c d e ts')

{-
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
-}
data Bool = True | False
  deriving (Show)

myThreadId :: IO ThreadId
myThreadId = IO (\tid ts exc cont -> cont ts tid)

execIO :: IO a -> Prelude.IO a
execIO io = do
    status <- Prelude.newIORef (ThreadRunning (Prelude.error "no error handler associated with thread."))
    let mainThreadId = ThreadId status
    runIO io mainThreadId emptyThreadList missingExceptionHandler (\_ -> return)
  where
    missingExceptionHandler = undefined

data SomeException = SomeException String

newtype IO a = IO { runIO :: forall r. ThreadId -> ThreadList
                          -> (ThreadList -> SomeException -> Prelude.IO r)
                          -> (ThreadList -> a -> Prelude.IO r)
                          -> Prelude.IO r }

instance Monad IO where
  return a = IO (\tid ts _exc cont -> cont ts a)
  (>>=) f g = IO (\tid !ts exc cont -> runIO f tid ts exc (\ts' a -> runIO (g a) tid ts' exc cont))

liftIO :: Prelude.IO a -> IO a
liftIO action = IO (\tid ts exc cont -> action >>= cont ts)

{-
terminate :: Bool -> IO ()
terminate False = return ()
terminate True  = IO (\_ _ -> Terminate)

data AnyIOPrim = forall r. AnyIOPrim { anyIOPrim :: !(IOPrim r) }

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

  ASyncSpawn :: forall a. IOPrim a -> IOPrim ()
  Yield      :: IOPrim a

  -- Exception handling
  --ThrowIO :: SomeException -> IOPrim a
  --ThrowTo :: ThreadId -> SomeException -> IOPrim a
  --Catch :: (SomeException -> IOPrim a) -> IOPrim a -> IOPrim a

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
      --Prelude.putStrLn "Spawn"
      pushThread prim
    Yield -> do
      thread <- popThread
      --Prelude.putStrLn "Yield"
      case thread of
        AnyIOPrim prim -> runIOPrim prim >> runIOPrim Yield



-}
putStrLn :: String -> IO ()
putStrLn str = liftIO (Prelude.putStrLn str)

-- Assume IORefs
type IORef a = Prelude.IORef a

newIORef :: a -> IO (IORef a)
newIORef a = liftIO (Prelude.newIORef a)

readIORef :: IORef a -> IO a
readIORef ref = liftIO (Prelude.readIORef ref)

writeIORef :: IORef a -> a -> IO ()
writeIORef ref a = liftIO (Prelude.writeIORef ref a)


newtype MVar a = MVar (IORef (MVarContent a))
data MVarContent a =
  MVarEmpty |
  MVarSingleReader !(a -> ThreadList -> Prelude.IO ())  |
  --MVarReaders [a -> ThreadList -> Prelude.IO ()] |
  MVarFull a
  --MVarWriters a [(a, ThreadList -> Prelude.IO ())]

newEmptyMVar :: IO (MVar a)
newEmptyMVar = do
  ref <- newIORef MVarEmpty
  return (MVar ref)

takeMVar :: MVar a -> IO a
takeMVar (MVar ref) = IO (\tid ts exc cont -> do
  content <- Prelude.readIORef ref
  case content of
    MVarEmpty -> do
      let elt = (\a ts -> cheat (cont ts a) )
      Prelude.writeIORef ref (MVarSingleReader elt)
      yieldIO ts
    {-MVarSingleReader elt -> do
      let lst' = (\a ts -> cheat (cont ts a) ) : [elt]
      Prelude.writeIORef ref (MVarReaders lst')
      yieldIO ts-}
    {-MVarReaders lst -> do
      let lst' = (\a ts -> cheat (cont ts a) ) : lst
      Prelude.writeIORef ref (MVarReaders lst')
      yieldIO ts-}
    MVarFull a -> do
      Prelude.writeIORef ref MVarEmpty
      cont ts a
    {-MVarWriters a [] -> do
      Prelude.writeIORef ref MVarEmpty 
      cont ts a
    MVarWriters a [(a',x)] -> do
      Prelude.writeIORef ref (MVarFull a')
      cont ts a
    MVarWriters a ((a',x):xs) -> do
      Prelude.writeIORef ref (MVarWriters a' xs)
      cont ts a-}
  )

putMVar :: MVar a -> a -> IO ()
putMVar (MVar ref) a = IO (\tid ts exc cont -> do
  content <- Prelude.readIORef ref
  case content of
    MVarEmpty -> do
      Prelude.writeIORef ref (MVarFull a)
      cont ts ()
    {-MVarReaders [] -> do
      Prelude.writeIORef ref (MVarFull a)
      cont ts ()-}
    MVarSingleReader x -> do
      Prelude.writeIORef ref MVarEmpty
      cont (pushThread (x a) ts) ()
    {-MVarReaders (x:xs) -> do
      Prelude.writeIORef ref (MVarReaders xs) 
      cont (pushThread (x a) ts) ()-}
    {-MVarFull orig -> do
      Prelude.writeIORef ref (MVarWriters orig [(a, \ts -> cheat (cont ts ()) ) ]) 
      yieldIO ts-}
    {-MVarWriters orig lst -> do
      Prelude.writeIORef ref (MVarWriters orig ((a, \ts -> cheat (cont ts ()) ) : lst))
      yieldIO ts-}
  )

cheat :: Prelude.IO a -> Prelude.IO b
cheat = unsafeCoerce#

spawn :: IO () -> IO ()
spawn io = IO (\tid ts exc cont -> cont (pushThread (\ts -> runIO io (ThreadId undefined) ts undefined (\ts () -> yieldIO ts)) ts) ())
--spawn io = IO (\exc cont -> ASyncSpawn ((runIO io missingExceptionHandler Unit))
--                            `Bind` cont)

--yield :: IO a
--yield = IO (\ts exc cont -> yieldIO ts)

yieldIO :: ThreadList -> Prelude.IO a
yieldIO ts = cheat (popThread ts)
--yieldIO (ThreadList [])     = Prelude.error "All threads have finished."
--yieldIO (ThreadList (x:xs)) = do
--    cheat (x (ThreadList xs))
{-

missingExceptionHandler :: SomeException -> IOPrim a
missingExceptionHandler (SomeException val) =
  PutStrLn ("Exception reached top-level without being catched: " ++ show val)
  `Bind` \() -> Terminate
-}

testProgram :: IO ()
testProgram = do
  ref <- newEmptyMVar
  spawn (putMVar ref True)
  val <- takeMVar ref
  putStrLn ("Boolean: " ++ show val)
{-
testException :: IO String
testException = do
  throwIO (SomeException True)
    `catch` \_ -> return "Caught exception."
-}

testThrowTo :: IO ()
testThrowTo = do
  tid <- myThreadId
  handle (\(SomeException e) -> putStrLn ("E: " ++ show e))
    (do putStrLn "Inside exception handler."
        throwTo tid (SomeException "Catch me!")
        putStrLn "This should not be seen."
    )
  putStrLn "This should be seen."

testUserThread :: Int -> IO ()
testUserThread n = do
  e <- newEmptyMVar
  o <- createMany n e
  putMVar e 0
  l <- takeMVar o
  putStrLn ("Out: " ++ show l)

createMany :: Int -> MVar Int -> IO (MVar Int)
createMany 0 v = return v
createMany n v = do
  o <- newEmptyMVar
  spawn (copy v o)
  createMany (n-1) o

copy :: MVar Int -> MVar Int -> IO ()
copy i o = do
  n <- takeMVar i
  let n'  = n+1
  seq n' (putMVar o n')
  copy i o
{-
testYield :: IO Bool
testYield = IO (\ext cont -> Yield)

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

-}
throwIO :: SomeException -> IO a
throwIO e = IO (\tid ts exc _ -> exc ts e)

--throw :: SomeException -> a
--throw e = mkContinuation (\exc _ -> exc e)

handle :: (SomeException -> IO a) -> IO a -> IO a
handle = Prelude.flip catch

catch :: IO a -> (SomeException -> IO a) -> IO a
catch action handler = IO (\tid@(ThreadId ref) ts exc cont -> do
    status <- Prelude.readIORef ref
    let restore = Prelude.writeIORef ref status
    Prelude.writeIORef ref (ThreadRunning (\ts' e -> restore >> cheat (runIO (handler e) tid ts' exc cont)))
    runIO action tid ts (\ts' e -> restore >> runIO (handler e) tid ts' exc cont ) (\ts val -> restore >> cont ts val)
  )

-- Primitive.
--foreign import ccall mkContinuation :: ((a -> IOPrim r) -> IOPrim r) -> a
--mkContinuation :: ( (SomeException -> IOPrim r) ->
--                    (a -> IOPrim r) ->
--                    IOPrim r)
--               -> a
--mkContinuation = undefined


--unsafePerformIO :: IO a -> a
--unsafePerformIO io = mkContinuation (\exc cont -> runIO io exc cont)

{-

{-
ioFromPure cont = runIO getLine cont
-}
data Addr = Addr
gc_alloc :: (Addr -> IOPrim a) -> IOPrim a
gc_alloc cont = undefined

-}




