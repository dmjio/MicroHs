-- Callbacks from JavaScript into Haskell, exceptions and threads (emscripten target).
module JSCallback(main) where
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List(isInfixOf)
import GHC.Wasm.Prim

-- Call the callback from the JavaScript event loop (after ms milliseconds).
foreign import javascript "setTimeout(() => $1(), $2)" later :: JSVal -> Int -> IO ()
-- Call the callback from a JavaScript timer, and report an exception thrown by it.
foreign import javascript "setTimeout(() => { try { $1() } catch (e) { console.log('JavaScript caught: ' + e.message) } }, $2)"
  laterCatch :: JSVal -> Int -> IO ()
-- Call an asynchronous callback (returns a Promise) and report a rejection.
foreign import javascript "$1().then(() => console.log('promise resolved'), e => console.log('promise rejected: ' + e.message))"
  callAsync :: JSVal -> IO ()
-- A Promise that resolves after ms milliseconds.
foreign import javascript "new Promise(r => setTimeout(() => r(42), $1))" delayed :: Int -> IO JSVal
foreign import javascript interruptible "return await $1" await :: JSVal -> IO JSVal
foreign import javascript "$1" jsValToInt :: JSVal -> IO Int
foreign import javascript "({ n: $1 })" mkObj :: Int -> IO JSVal
foreign import javascript safe "$1.n" getN :: JSVal -> IO Int
foreign import javascript "mhsjs.kv.size" tableSize :: IO Int

main :: IO ()
main = do
  mainTid <- myThreadId
  mv <- newEmptyMVar

  -- 1. A callback runs as a thread of its own while main is blocked in takeMVar:
  --    catch, masking state, myThreadId and forkIO work.
  cb1 <- syncCallback $ do
    r <- try (evaluate (1 `div` (0 :: Int)))
    putStrLn $ "callback: " ++ either (\ e -> "caught " ++ show (e :: ArithException)) (const "no exception") r
    st <- getMaskingState
    putStrLn $ "callback: masking state " ++ show st
    tid <- myThreadId
    putStrLn $ "callback: own thread " ++ show (tid /= mainTid)
    _ <- forkIO $ putStrLn "forked thread ran"
    putMVar mv "callback 1"
  later cb1 10
  putStrLn "main: waiting"
  takeMVar mv >>= putStrLn . ("main: got " ++)

  -- 2. An uncaught exception in a callback becomes a JavaScript Error; the runtime continues.
  cb2 <- syncCallback $ do
    putStrLn "callback: throwing"
    _ <- throwIO (ErrorCall "boom")
    putMVar mv "unreachable"
  cb3 <- syncCallback $ putMVar mv "callback 3"
  laterCatch cb2 10
  later cb3 20
  takeMVar mv >>= putStrLn . ("main: got " ++)

  -- 3. An asynchronous callback that throws rejects its Promise.
  acb <- asyncCallback $ throwIO (ErrorCall "async boom")
  callAsync acb
  acb2 <- asyncCallback $ putMVar mv "callback 4"
  callAsync acb2
  takeMVar mv >>= putStrLn . ("main: got " ++)

  -- 4. killThread from a callback reaches a thread sleeping in threadDelay while main is blocked.
  t <- forkIO $ (threadDelay 5000000 >> putStrLn "not killed")
                  `catch` \ e -> putStrLn ("thread got: " ++ show (e :: AsyncException))
  cb4 <- syncCallback $ killThread t >> putMVar mv "callback 5"
  later cb4 10
  takeMVar mv >>= putStrLn . ("main: got " ++)

  -- 5. A callback runs while main awaits a Promise (interruptible import), and an
  --    interruptible import from the callback raises a JSException.
  cb5 <- syncCallback $ do
    putStrLn "callback: during await"
    r <- try (delayed 1 >>= await)
    case r of
      Left e -> putStrLn $ "callback: " ++ (if "asynchronous operation" `isInfixOf` show (e :: JSException) then "caught JSException for nested await" else show e)
      Right _ -> putStrLn "callback: nested await worked?"
  later cb5 10
  v <- await =<< delayed 50
  jsValToInt v >>= \ n -> putStrLn ("main: await done " ++ show n)

  -- 6. Freeing a JSVal twice is harmless; using it afterwards is a JSException with safe.
  o <- mkObj 7
  freeJSVal o
  freeJSVal o
  r <- try (getN o)
  putStrLn $ either (\ e -> if "freed" `isInfixOf` show (e :: JSException) then "use after free: caught" else show e) (const "use after free: no exception?") r

  -- 7. Many short-lived JSVals do not grow the handle table without bound
  --    (the GC is forced after JSVAL_GC_LIMIT new handles).
  s0 <- tableSize
  forM_ [1 .. 150000 :: Int] $ \ i -> mkObj i >>= getN >>= \ n -> when (n /= i) (putStrLn "bad n")
  s1 <- tableSize
  putStrLn $ if s1 - s0 < 150000 then "handle table bounded" else "handle table grew by " ++ show (s1 - s0)

  putStrLn "main: done"
