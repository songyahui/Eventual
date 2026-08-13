{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.Memory where
import Prelude hiding ((<>))
import Pledge
import System.Random

-- free requires that malloc was the immediately preceding post-event.
-- For interleaved mallocs use `pre = universe` and rely on `future` instead.

type RETerm = (RE Term)

malloc :: IO (Pledge RETerm Addr)
malloc = do
    let randomNum :: IO Int = randomRIO (1, 100)
    addr <- randomNum
    return $ Pledge
        { ret    = addr
        , pre    = universe
        , post   = Single (Atom "malloc" (List [Num addr]))
        , future = finally (Atom "free" (List [Num addr]))
        }

free :: Addr -> Pledge RETerm ()
free addr = Pledge
    { ret    = ()
    , pre    = previously (Atom "malloc" (List [Num addr]))
    , post   = Single (Atom "free" (List [Num addr]))
    -- noUntil free malloc: free(addr) must not occur again until malloc(addr)
    -- happens first.  This prevents double-free while allowing re-allocation:
    --   free → free          is forbidden  (double-free, no malloc in between)
    --   free → malloc → free is allowed    (re-allocation is valid)
    , future = noUntil (Atom "free" (List [Num addr]))
                               (Atom "malloc" (List [Num addr]))
    }

-- Good: malloc uses the returned address to parameterise the free obligation.
-- Demonstrates data-dependent future: future = \a -> finally(free(a)).
mallocFreeByReturnedAddr :: IO (Pledge RETerm ())
mallocFreeByReturnedAddr = do
    paddr :: Pledge RETerm Addr <- malloc
    return $ paddr >>= free


-- Good: two sequential malloc/free pairs — each obligation is discharged in turn.
mallocFreeSequential :: IO (Pledge RETerm ())
mallocFreeSequential = do
    paddr1 :: Pledge RETerm Addr <- malloc
    paddr2 :: Pledge RETerm Addr <- malloc
    return $ (paddr1 >>= free) >> (paddr2 >>= free)

-- Good: malloc and free every address in a loop.
loopAllFreed :: Int -> IO [Pledge RETerm ()]
loopAllFreed n = mapM (const eachRun) [1..n]
    where
    eachRun :: IO (Pledge RETerm ())
    eachRun = do
        paddr :: Pledge RETerm Addr <- malloc
        return $ paddr >>= free

-- Bad: malloc 1 and 2, only free 1 — future obligation for address 2 remains.
missingFree :: IO (Pledge RETerm Addr)
missingFree = do
    paddr1 :: Pledge RETerm Addr <- malloc
    paddr2 :: Pledge RETerm Addr <- malloc
    return $ (paddr1 >>= free) >> paddr2


-- Bad: free without a preceding malloc — precondition violated (pre = Bot).
freeWithoutMalloc :: IO (Pledge RETerm ())
freeWithoutMalloc = return $ free 1

-- Bad: allocate via malloc, free the wrong address.
-- future of (malloc 1) evaluates to finally(free(1));
-- free 2 does not discharge it, so finally(free(1)) remains.
wrongAddrFree :: IO (Pledge RETerm ())
wrongAddrFree = do
    paddr :: Pledge RETerm Addr <- malloc
    return $ paddr >>= \n -> free (n+1)


-- Bad: free the same address twice immediately.
-- After the first free, future = noUntil(free(1), malloc(1)).
-- The second free posts free(1) before any malloc(1), so the future becomes ∅.
doubleFreeImmediate :: IO (Pledge RETerm ())
doubleFreeImmediate = do
    paddr :: Pledge RETerm Addr <- malloc
    return $ paddr >>= \n -> do free n >> free n

-- Bad: double-free with intervening work between the two frees.
-- The noUntil(free(1), malloc(1)) future set by the first free is still active
-- when the second free arrives, regardless of what happens in between.
doubleFreeWithWork :: IO (Pledge RETerm ())
doubleFreeWithWork = do
    paddr1 :: Pledge RETerm Addr <- malloc
    paddr2 :: Pledge RETerm Addr <- malloc
    return $ do
        addr1 <- paddr1
        addr2 <- paddr2
        free addr1
        free addr2
        free addr1

-- Bad: leak combined with double-free in the same program.
-- addr 2 is never freed (leak) and addr 1 is freed twice (double-free).
-- Both violations are captured: future carries ∅ from the double-free.
leakAndDoubleFree :: IO (Pledge RETerm ())
leakAndDoubleFree = do
    paddr1 :: Pledge RETerm Addr <- malloc
    paddr2 :: Pledge RETerm Addr <- malloc
    return $ do
        addr1 <- paddr1
        _ <- paddr2
        free addr1
        free addr1

-- Good: reallocate the same address after freeing it.
-- After free 1: future = noUntil(free(1), malloc(1)).
-- malloc 1 resets the guard (derivative w.r.t. malloc(1) = Σ*).
-- malloc 1 also imposes finally(free(1)), which the second free discharges.
mallocFreeReallocFree :: IO (Pledge RETerm ())
mallocFreeReallocFree = do
    paddr :: Pledge RETerm Addr <- malloc
    return $ (paddr >>= free) >> (paddr >>= free)

-- Good: allocate and return the address without freeing.
-- ret = 1; future = finally(free(1)) — obligation visible in the residual.
allocReturnAddr :: IO (Pledge RETerm Addr)
allocReturnAddr = malloc

-- Bad: allocate two addresses and return both — neither obligation is discharged.
-- ret = (1, 2); future = finally(free(1)) ∧ finally(free(2)).
allocTwoReturnPair :: IO (Pledge RETerm (Addr, Addr))
allocTwoReturnPair = do
    paddr1 :: Pledge RETerm Addr <- malloc
    paddr2 :: Pledge RETerm Addr <- malloc
    return $
        Pledge {
            ret = (ret paddr1, ret paddr2),
            pre = pre paddr1 /\ pre paddr2,
            post = post paddr1 · post paddr2,
            future = future paddr1 /\ future paddr2 }

-- Good: allocate, free, and return the freed address.
-- ret = 1; future = noUntil(free(1), malloc(1)) — guard active, no pending finally.
allocFreeReturnAddr :: IO (Pledge RETerm Addr)
allocFreeReturnAddr = do
    paddr :: Pledge RETerm Addr <- malloc
    let base :: Pledge RETerm () = paddr >>= \n -> free n
    return $
        Pledge {
            ret = ret paddr,
            pre = pre base,
            post = post base,
            future = future base }

main :: IO ()
main = do
    mallocFreeByReturnedAddr' <- mallocFreeByReturnedAddr
    printOfPledgeRE "mallocFreeByReturnedAddr" mallocFreeByReturnedAddr'
    mallocFreeSequential' <- mallocFreeSequential
    printOfPledgeRE "mallocFreeSequential"     mallocFreeSequential'
    loopAllFreed' <- loopAllFreed 3
    mapM_ (printOfPledgeRE "loopAllFreed 3")   loopAllFreed'
    missingFree' :: Pledge RETerm Addr <- missingFree
    printOfPledgeRE "missingFree"              missingFree'
    freeWithoutMalloc' <- freeWithoutMalloc
    printOfPledgeRE "freeWithoutMalloc"        freeWithoutMalloc'
    wrongAddrFree' <- wrongAddrFree
    printOfPledgeRE "wrongAddrFree"            wrongAddrFree'
    doubleFreeImmediate' <- doubleFreeImmediate
    printOfPledgeRE "doubleFreeImmediate"      doubleFreeImmediate'
    doubleFreeWithWork' <- doubleFreeWithWork
    printOfPledgeRE "doubleFreeWithWork"       doubleFreeWithWork'
    leakAndDoubleFree' <- leakAndDoubleFree
    printOfPledgeRE "leakAndDoubleFree"        leakAndDoubleFree'
    mallocFreeReallocFree' <- mallocFreeReallocFree
    printOfPledgeRE "mallocFreeReallocFree"    mallocFreeReallocFree'
    allocReturnAddr' <- allocReturnAddr
    printOfPledgeRE "allocReturnAddr"          allocReturnAddr'
    allocTwoReturnPair' <- allocTwoReturnPair
    printOfPledgeRE "allocTwoReturnPair"       allocTwoReturnPair'
    allocFreeReturnAddr' <- allocFreeReturnAddr
    printOfPledgeRE "allocFreeReturnAddr"      allocFreeReturnAddr'
    return ()
