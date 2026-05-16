{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.Mutex where
import Prelude hiding ((<>))
import FutureCond

acquire :: Int -> FutureCond RE ()
acquire mid = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "acquire" (List [Num mid]))
    , future = \_ -> finally (Atom "release" (List [Num mid]))
    }

-- Precondition: acquire(mid) must have been the immediately preceding event
release :: Int -> FutureCond RE ()
release mid = FutureCond
    { ret    = ()
    , pre    = Single (Atom "acquire" (List [Num mid]))
    , post   = Single (Atom "release" (List [Num mid]))
    , future = \_ -> universe
    }

criticalWork :: FutureCond RE ()
criticalWork = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "work" (List []))
    , future = \_ -> universe
    }

-- Good: acquire, work, release
safeSection :: FutureCond RE ()
safeSection = do
    acquire 1
    release 1

-- Good: nested locks, released in reverse order
nestedLocks :: FutureCond RE ()
nestedLocks = do
    acquire 1
    acquire 2
    release 2
    release 1

-- Bad: acquire two locks, release only one — lock 1 future obligation remains
lockLeak :: FutureCond RE ()
lockLeak = do
    acquire 1
    acquire 2
    release 2
    -- release 1 missing

-- Bad: release without acquire — precondition violated (pre = Bot)
releaseWithoutAcquire :: FutureCond RE ()
releaseWithoutAcquire = release 1

printResult :: String -> FutureCond RE () -> IO ()
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
