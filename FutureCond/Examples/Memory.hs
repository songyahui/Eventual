{-# OPTIONS_GHC -i.. #-}
module Examples.Memory where
import Prelude hiding ((<>))
import Future

-- free requires that malloc was the immediately preceding post-event.
-- For interleaved mallocs use `pre = universe` and rely on `future` instead.

malloc :: Int -> Effectful RE ()
malloc addr = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "malloc" (List [Num addr]))
    , future = finally (Atom "free" (List [Num addr]))
    }

free :: Int -> Effectful RE ()
free addr = Effectful
    { ret    = ()
    , pre    = Single (Atom "malloc" (List [Num addr]))   -- malloc(addr) must have just occurred
    , post   = Single (Atom "free" (List [Num addr]))
    , future = universe
    }

-- Good: malloc then immediately free — precondition satisfied, future discharged
simpleFree :: Effectful RE ()
simpleFree = do
    malloc 1
    free 1

-- Good: malloc and free every address (interleaved; future tracks both obligations)
good :: Effectful RE ()
good = do
    malloc 1
    free 1
    malloc 2
    free 2

-- Good: loop
loopAllFreed :: Int -> Effectful RE ()
loopAllFreed n = foldr (>>) (return ()) [malloc i >> free i | i <- [1..n]]

-- Bad: malloc 1 and 2, only free 1 — future obligation for address 2 remains
leak :: Effectful RE ()
leak = do
    malloc 1
    free 1
    malloc 2       -- future: free(2) pending

-- Bad: free without a preceding malloc — precondition violated (pre = Bot)
freeWithoutMalloc :: Effectful RE ()
freeWithoutMalloc = free 1

printResult :: String -> Effectful RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre    prog))
    putStrLn $ "Post:   " ++ show (normalize (post   prog))
    putStrLn $ "Future: " ++ show (normalize (future prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "simpleFree"           simpleFree
    printResult "good (free after each malloc)"  good
    printResult "loopAllFreed 3"       (loopAllFreed 3)
    printResult "leak (malloc 2 not freed)"      leak
    printResult "freeWithoutMalloc"    freeWithoutMalloc
