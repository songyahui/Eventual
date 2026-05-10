{-# OPTIONS_GHC -i.. #-}
module Examples.Mutex where
import Prelude hiding ((<>))
import Future

acquire :: Int -> Effectful RE ()
acquire mid = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "acquire" (List [Num mid]))
    , future = finally (Atom "release" (List [Num mid]))
    }

-- Precondition: acquire(mid) must have been the immediately preceding event
release :: Int -> Effectful RE ()
release mid = Effectful
    { ret    = ()
    , pre    = Single (Atom "acquire" (List [Num mid]))
    , post   = Single (Atom "release" (List [Num mid]))
    , future = universe
    }

criticalWork :: Effectful RE ()
criticalWork = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "work" (List []))
    , future = universe
    }

-- Good: acquire, work, release
safeSection :: Effectful RE ()
safeSection = do
    acquire 1
    release 1

-- Good: nested locks, released in reverse order
nestedLocks :: Effectful RE ()
nestedLocks = do
    acquire 1
    acquire 2
    release 2
    release 1

-- Bad: acquire two locks, release only one — lock 1 future obligation remains
lockLeak :: Effectful RE ()
lockLeak = do
    acquire 1
    acquire 2
    release 2
    -- release 1 missing

-- Bad: release without acquire — precondition violated (pre = Bot)
releaseWithoutAcquire :: Effectful RE ()
releaseWithoutAcquire = release 1

printResult :: String -> Effectful RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre    prog))
    putStrLn $ "Post:   " ++ show (normalize (post   prog))
    putStrLn $ "Future: " ++ show (normalize (future prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "safeSection"          safeSection
    printResult "nestedLocks"          nestedLocks
    printResult "lockLeak"             lockLeak
    printResult "releaseWithoutAcquire" releaseWithoutAcquire
