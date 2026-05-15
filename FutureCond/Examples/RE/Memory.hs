{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.Memory where
import Prelude hiding ((<>))
import Future

-- free requires that malloc was the immediately preceding post-event.
-- For interleaved mallocs use `pre = universe` and rely on `future` instead.

malloc :: Addr -> Effectful RE Addr
malloc addr = Effectful
    { ret    = addr
    , pre    = universe
    , post   = Single (Atom "malloc" (List [Num addr]))
    , future = \a -> finally (Atom "free" (List [Num a]))
    }

free :: Addr -> Effectful RE ()
free addr = Effectful
    { ret    = ()
    , pre    = previously (Atom "malloc" (List [Num addr]))
    , post   = Single (Atom "free" (List [Num addr]))
    -- noUntil free malloc: free(addr) must not occur again until malloc(addr)
    -- happens first.  This prevents double-free while allowing re-allocation:
    --   free → free          is forbidden  (double-free, no malloc in between)
    --   free → malloc → free is allowed    (re-allocation is valid)
    , future = \_ -> noUntil (Atom "free" (List [Num addr]))
                               (Atom "malloc" (List [Num addr]))
    }

-- Good: malloc then immediately free — precondition satisfied, future discharged
dataDependentFree :: Addr -> Effectful RE ()
dataDependentFree n = do
    addr <- malloc n
    free addr

-- Good: malloc and free every address (interleaved; future tracks both obligations)
good :: Effectful RE ()
good = do
    a1 <- malloc 1
    free a1
    a2 <- malloc 2
    free a2

-- Good: loop
loopAllFreed :: Int -> Effectful RE ()
loopAllFreed n = foldr (>>) (return ()) [malloc i >>= free | i <- [1..n]]

-- Bad: malloc 1 and 2, only free 1 — future obligation for address 2 remains
leak :: Effectful RE ()
leak = do
    a1 <- malloc 1
    free a1
    _ <- malloc 2       -- future: free(2) pending
    return ()

-- Bad: free without a preceding malloc — precondition violated (pre = Bot)
freeWithoutMalloc :: Effectful RE ()
freeWithoutMalloc = free 1

-- Bad: free the same address twice — second free violates future of first free.
-- After the first free, future = never(free(1)).
-- The second free posts free(1), so the residual future becomes ∅ (violated).
doubleFree :: Effectful RE ()
doubleFree = do
    addr <- malloc 1
    free addr
    free addr   -- future of first free is never(free(1)); this triggers it

-- Bad: double-free with intervening work between the two frees.
-- The never(free(1)) future set by the first free is still active when the
-- second free arrives, regardless of what happens in between.
doubleFreeWithWork :: Effectful RE ()
doubleFreeWithWork = do
    a1 <- malloc 1
    a2 <- malloc 2   -- some unrelated allocation in between
    free a1
    free a2
    free a1          -- double-free: future = ∅

-- Bad: two addresses allocated; only address 1 is double-freed.
-- future of (free 2) is still never(free(2)) — undischarged but not violated.
-- future of the whole program collapses to ∅ because of the double-free of 1.
doubleFreeOneOfTwo :: Effectful RE ()
doubleFreeOneOfTwo = do
    a1 <- malloc 1
    a2 <- malloc 2
    free a1
    free a2

-- Bad: leak combined with double-free in the same program.
-- addr 2 is never freed (leak) and addr 1 is freed twice (double-free).
-- Both violations are captured: future carries ∅ from the double-free.
leakAndDoubleFree :: Effectful RE ()
leakAndDoubleFree = do
    a1 <- malloc 1
    _  <- malloc 2   -- addr 2 is never freed
    free a1
    free a1          -- double-free of addr 1

-- Good: reallocate the same address after freeing it.
-- After free 1: future = noUntil(free(1), malloc(1)).
-- malloc 1 resets the guard: noUntil(...) after consuming malloc(1) = Σ*.
-- malloc 1 also imposes finally(free(1)), which the second free discharges.
reallocSameAddr :: Effectful RE ()
reallocSameAddr = do
    a1 <- malloc 1
    free a1
    a1 <- malloc 1   -- re-allocate addr 1: noUntil guard is reset by malloc
    free a1          -- obligation discharged; future = noUntil(free(1), malloc(1))

-- Bad: allocate via malloc, free the wrong address.
-- future of (malloc 1) evaluates to finally(free(1));
-- free 2 does not discharge it, so finally(free(1)) remains.
wrongAddrFree :: Effectful RE ()
wrongAddrFree = do
    addr <- malloc 1
    free (addr + 1)   -- frees address 2, obligation for address 1 remains

printResult :: forall a. String -> Effectful RE a -> IO a
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre    prog))
    putStrLn $ "Post:   " ++ show (normalize (post   prog))
    putStrLn $ "Future: " ++ show (normalize (evalFuture prog))
    return (ret prog)

main :: IO ()
main = do
    printResult "dataDependentFree"    (dataDependentFree 1)
    printResult "good (free after each malloc)"  good
    printResult "loopAllFreed 3"       (loopAllFreed 3)
    printResult "leak (malloc 2 not freed)"      leak
    printResult "freeWithoutMalloc"    freeWithoutMalloc
    printResult "wrongAddrFree"        wrongAddrFree
    printResult "doubleFree"              doubleFree
    printResult "doubleFreeWithWork"      doubleFreeWithWork
    printResult "doubleFreeOneOfTwo"      doubleFreeOneOfTwo
    printResult "leakAndDoubleFree"       leakAndDoubleFree
    printResult "reallocSameAddr"         reallocSameAddr
