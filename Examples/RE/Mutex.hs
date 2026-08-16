{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.Mutex where
import Prelude hiding ((<>))
import Pledge

acquire :: Int -> Pledge IO (RE Term) ()
acquire mid = Pledge $ return
    ((), universe,
     Single (Atom "acquire" (List [Num mid])),
     finally (Atom "release" (List [Num mid])))

-- Precondition: acquire(mid) must have been the immediately preceding event
release :: Int -> Pledge IO (RE Term) ()
release mid = Pledge $ return
    ((), Single (Atom "acquire" (List [Num mid])),
     Single (Atom "release" (List [Num mid])),
     universe)

criticalWork :: Pledge IO (RE Term) ()
criticalWork = Pledge $ return
    ((), universe, Single (Atom "work" (List [])), universe)

-- Good: acquire, work, release
safeSection :: Pledge IO (RE Term) ()
safeSection = do
    acquire 1
    release 1

-- Good: nested locks, released in reverse order
nestedLocks :: Pledge IO (RE Term) ()
nestedLocks = do
    acquire 1
    acquire 2
    release 2
    release 1

-- Bad: acquire two locks, release only one — lock 1 future obligation remains
lockLeak :: Pledge IO (RE Term) ()
lockLeak = do
    acquire 1
    acquire 2
    release 2
    -- release 1 missing

-- Bad: release without acquire — precondition violated (pre = Bot)
releaseWithoutAcquire :: Pledge IO (RE Term) ()
releaseWithoutAcquire = release 1

printResult :: String -> Pledge IO (RE Term) () -> IO ()
printResult name prog = do
    (_, preC, postC, futC) <- runPledge prog
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize preC)
    putStrLn $ "Post:   " ++ show (normalize postC)
    putStrLn $ "Future: " ++ show (normalize futC)
    putStrLn ""

main :: IO ()
main = do
    printResult "safeSection"          safeSection
    printResult "nestedLocks"          nestedLocks
    printResult "lockLeak"             lockLeak
    printResult "releaseWithoutAcquire" releaseWithoutAcquire
