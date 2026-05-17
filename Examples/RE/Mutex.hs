{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.Mutex where
import Prelude hiding ((<>))
import Pledge

acquire :: Int -> Pledge RE ()
acquire mid = Pledge
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "acquire" (List [Num mid]))
    , future = \_ -> finally (Atom "release" (List [Num mid]))
    }

-- Precondition: acquire(mid) must have been the immediately preceding event
release :: Int -> Pledge RE ()
release mid = Pledge
    { ret    = ()
    , pre    = Single (Atom "acquire" (List [Num mid]))
    , post   = Single (Atom "release" (List [Num mid]))
    , future = const universe
    }

criticalWork :: Pledge RE ()
criticalWork = Pledge
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "work" (List []))
    , future = const universe
    }

-- Good: acquire, work, release
safeSection :: Pledge RE ()
safeSection = do
    acquire 1
    release 1

-- Good: nested locks, released in reverse order
nestedLocks :: Pledge RE ()
nestedLocks = do
    acquire 1
    acquire 2
    release 2
    release 1

-- Bad: acquire two locks, release only one — lock 1 future obligation remains
lockLeak :: Pledge RE ()
lockLeak = do
    acquire 1
    acquire 2
    release 2
    -- release 1 missing

-- Bad: release without acquire — precondition violated (pre = Bot)
releaseWithoutAcquire :: Pledge RE ()
releaseWithoutAcquire = release 1

printResult :: String -> Pledge RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre    prog))
    putStrLn $ "Post:   " ++ show (normalize (post   prog))
    putStrLn $ "Future: " ++ show (normalize (evalFuture prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "safeSection"          safeSection
    printResult "nestedLocks"          nestedLocks
    printResult "lockLeak"             lockLeak
    printResult "releaseWithoutAcquire" releaseWithoutAcquire
