{-# OPTIONS_GHC -i.. #-}
module Examples.Mutex where
import Prelude hiding ((<>))
import Future

-- Acquire a mutex: future = eventually release(id)
acquire :: Int -> Effectful RE ()
acquire id = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("acquire", [Num id])
    , future = finally ("release", [Num id])
    }

-- Release a mutex: discharges future obligation
release :: Int -> Effectful RE ()
release id = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("release", [Num id])
    , future = anything
    }

-- Critical section work: no obligation
criticalWork :: Effectful RE ()
criticalWork = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("work", [])
    , future = anything
    }

-- Good: acquire, work, release
safeSection :: Effectful RE ()
safeSection = do
    acquire 1
    criticalWork
    release 1

-- Good: nested locks, both released in order
nestedLocks :: Effectful RE ()
nestedLocks = do
    acquire 1
    acquire 2
    criticalWork
    release 2
    release 1

-- Bad: acquire two locks, release only one — lock 2 leaked
deadlockRisk :: Effectful RE ()
deadlockRisk = do
    acquire 1
    acquire 2
    criticalWork
    release 1

printResult :: String -> Effectful RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Post:   " ++ show (post prog)
    putStrLn $ "Future: " ++ show (normalize (future prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "safeSection"   safeSection
    printResult "nestedLocks"   nestedLocks
    printResult "deadlockRisk"  deadlockRisk
