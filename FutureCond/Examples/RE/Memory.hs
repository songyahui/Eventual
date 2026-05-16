{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.Memory where
import Prelude hiding ((<>))
import Pledge

-- free requires that malloc was the immediately preceding post-event.
-- For interleaved mallocs use `pre = universe` and rely on `future` instead.

malloc :: Addr -> Pledge RE Addr
malloc addr = Pledge
    { ret    = addr
    , pre    = universe
    , post   = Single (Atom "malloc" (List [Num addr]))
    , future = \a -> finally (Atom "free" (List [Num a]))
    }

free :: Addr -> Pledge RE ()
free addr = Pledge
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

-- Good: malloc uses the returned address to parameterise the free obligation.
-- Demonstrates data-dependent future: future = \a -> finally(free(a)).
mallocFreeByReturnedAddr :: Addr -> Pledge RE ()
mallocFreeByReturnedAddr n = do
    addr <- malloc n
    free addr

-- Good: two sequential malloc/free pairs — each obligation is discharged in turn.
mallocFreeSequential :: Pledge RE ()
mallocFreeSequential = do
    a1 <- malloc 1
    free a1
    a2 <- malloc 2
    free a2

-- Good: malloc and free every address in a loop.
loopAllFreed :: Int -> Pledge RE ()
loopAllFreed n = foldr (>>) (return ()) [malloc i >>= free | i <- [1..n]]

-- Bad: malloc 1 and 2, only free 1 — future obligation for address 2 remains.
missingFree :: Pledge RE ()
missingFree = do
    a1 <- malloc 1
    free a1
    _ <- malloc 2       -- future: free(2) pending
    return ()

-- Bad: free without a preceding malloc — precondition violated (pre = Bot).
freeWithoutMalloc :: Pledge RE ()
freeWithoutMalloc = free 1

-- Bad: allocate via malloc, free the wrong address.
-- future of (malloc 1) evaluates to finally(free(1));
-- free 2 does not discharge it, so finally(free(1)) remains.
wrongAddrFree :: Pledge RE ()
wrongAddrFree = do
    addr <- malloc 1
    free (addr + 1)   -- frees address 2, obligation for address 1 remains


-- Bad: free the same address twice immediately.
-- After the first free, future = noUntil(free(1), malloc(1)).
-- The second free posts free(1) before any malloc(1), so the future becomes ∅.
doubleFreeImmediate :: Pledge RE ()
doubleFreeImmediate = do
    addr <- malloc 1
    free addr
    free addr   -- noUntil guard triggered: future = ∅

-- Bad: double-free with intervening work between the two frees.
-- The noUntil(free(1), malloc(1)) future set by the first free is still active
-- when the second free arrives, regardless of what happens in between.
doubleFreeWithWork :: Pledge RE ()
doubleFreeWithWork = do
    a1 <- malloc 1
    a2 <- malloc 2   -- unrelated allocation in between
    free a1
    free a2
    free a1          -- double-free: future = ∅

-- Bad: leak combined with double-free in the same program.
-- addr 2 is never freed (leak) and addr 1 is freed twice (double-free).
-- Both violations are captured: future carries ∅ from the double-free.
leakAndDoubleFree :: Pledge RE ()
leakAndDoubleFree = do
    a1 <- malloc 1
    _  <- malloc 2   -- addr 2 is never freed
    free a1
    free a1          -- double-free of addr 1

-- Good: reallocate the same address after freeing it.
-- After free 1: future = noUntil(free(1), malloc(1)).
-- malloc 1 resets the guard (derivative w.r.t. malloc(1) = Σ*).
-- malloc 1 also imposes finally(free(1)), which the second free discharges.
mallocFreeReallocFree :: Pledge RE ()
mallocFreeReallocFree = do
    a1 <- malloc 1
    free a1
    a1 <- malloc 1   -- re-allocate addr 1: noUntil guard reset by malloc
    free a1          -- obligation discharged; future = noUntil(free(1), malloc(1))

-- Good: allocate and return the address without freeing.
-- ret = 1; future = finally(free(1)) — obligation visible in the residual.
allocReturnAddr :: Pledge RE Addr
allocReturnAddr = malloc 1

-- Bad: allocate two addresses and return both — neither obligation is discharged.
-- ret = (1, 2); future = finally(free(1)) ∧ finally(free(2)).
allocTwoReturnPair :: Pledge RE (Addr, Addr)
allocTwoReturnPair = do
    a1 <- malloc 1
    a2 <- malloc 2
    return (a1, a2)

-- Good: allocate, free, and return the freed address.
-- ret = 1; future = noUntil(free(1), malloc(1)) — guard active, no pending finally.
allocFreeReturnAddr :: Pledge RE Addr
allocFreeReturnAddr = do
    a <- malloc 1
    free a
    return a

printResult :: forall a. (Show a) => String -> Pledge RE a -> IO a
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre prog))
    putStrLn $ "Post:   " ++ show (normalize (post prog))
    putStrLn $ "Ret:    " ++ show (ret prog)
    putStrLn $ "Future: " ++ show (normalize (evalFuture prog))
    return (ret prog)

main :: IO ()
main = do
    printResult "mallocFreeByReturnedAddr" (mallocFreeByReturnedAddr 1)
    printResult "mallocFreeSequential"     mallocFreeSequential
    printResult "loopAllFreed 3"           (loopAllFreed 3)
    printResult "missingFree"              missingFree
    printResult "freeWithoutMalloc"        freeWithoutMalloc
    printResult "wrongAddrFree"            wrongAddrFree
    printResult "doubleFreeImmediate"      doubleFreeImmediate
    printResult "doubleFreeWithWork"       doubleFreeWithWork
    printResult "leakAndDoubleFree"        leakAndDoubleFree
    printResult "mallocFreeReallocFree"    mallocFreeReallocFree
    printResult "allocReturnAddr"          allocReturnAddr
    printResult "allocTwoReturnPair"       allocTwoReturnPair
    printResult "allocFreeReturnAddr"      allocFreeReturnAddr
    return ()
