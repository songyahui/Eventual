module Examples.GuardedRE.Memory where

import Prelude hiding ((<>))
import Pledge
import Pledge.GuardedRE

-- ── Model ─────────────────────────────────────────────────────────────────────
-- Combines two independent safety conditions on memory operations:
--
--   Trace constraint (RE):   malloc(addr) must precede free(addr);
--                            double-free is forbidden until re-allocation.
--   Heap constraint (PPred): h[addr] > 0 means the cell is live (allocated).
--                            A free requires h[addr] > 0; after free it drops to 0.
--
-- Using plain RE would miss the heap liveness check.
-- Using plain PPred would miss the trace ordering (malloc-before-free).
-- GuardedRE captures both simultaneously.

-- ── Primitives ────────────────────────────────────────────────────────────────

malloc :: Addr -> Pledge IO (GuardedRE Term) Addr
malloc addr = Pledge $ return
    ( addr
    , fromRE universe                                     -- pre: no precondition
    , fromRE (Single (Atom "malloc" (List [Num addr])))   -- post
      -- future: cell must be live AND free must eventually happen
    , GuardedRE (PGt (ValAt addr) (Lit 0))
                (finally (Atom "free" (List [Num addr])))
    )

free :: Addr -> Pledge IO (GuardedRE Term) ()
free addr = Pledge $ return
    ( ()
      -- pre: cell must be live (heap) AND malloc must have been observed (trace)
    , GuardedRE (PGt (ValAt addr) (Lit 0))
                (previously (Atom "malloc" (List [Num addr])))
    , fromRE (Single (Atom "free" (List [Num addr])))     -- post
      -- future: no double-free until re-allocation
    , fromRE (noUntil (Atom "free"   (List [Num addr]))
                      (Atom "malloc" (List [Num addr])))
    )

-- ── Programs ──────────────────────────────────────────────────────────────────

-- Good: malloc then free — both constraints satisfied.
mallocThenFree :: Pledge IO (GuardedRE Term) ()
mallocThenFree = do
    addr <- malloc 1
    free addr

-- Good: sequential pairs — each obligation discharged in turn.
sequential :: Pledge IO (GuardedRE Term) ()
sequential = do
    a1 <- malloc 1
    free a1
    a2 <- malloc 2
    free a2

-- Bad: missing free — future carries finally(free(1)) ∧ h[1] > 0.
missingFree :: Pledge IO (GuardedRE Term) ()
missingFree = do
    _ <- malloc 1
    return ()

-- Bad: free without malloc — pre contains previously(malloc(1)) which is not met.
freeWithoutMalloc :: Pledge IO (GuardedRE Term) ()
freeWithoutMalloc = free 1

-- Bad: double-free — noUntil guard triggers on the second free.
doubleFree :: Pledge IO (GuardedRE Term) ()
doubleFree = do
    addr <- malloc 1
    free addr
    free addr

-- Bad: free wrong address — future(malloc 1) = finally(free(1)) remains undischarged.
wrongFree :: Pledge IO (GuardedRE Term) ()
wrongFree = do
    addr <- malloc 1
    free (addr + 1)

-- Good: reallocate after free — noUntil guard reset by the second malloc.
reallocate :: Pledge IO (GuardedRE Term) ()
reallocate = do
    a <- malloc 1
    free a
    a' <- malloc 1
    free a'

-- ── Display ───────────────────────────────────────────────────────────────────

printResult :: forall a. Show a => String -> Pledge IO (GuardedRE Term) a -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    (_, preC, postC, futC) <- runPledge prog
    let GuardedRE prePred  preRE  = preC
        GuardedRE postPred postRE = postC
        GuardedRE futPred  futRE  = futC
    putStrLn $ "Pre:    [" ++ show prePred  ++ "]  " ++ show (normalize preRE)
    putStrLn $ "Post:   [" ++ show postPred ++ "]  " ++ show (normalize postRE)
    putStrLn $ "Future: [" ++ show futPred  ++ "]  " ++ show (normalize futRE)
    putStrLn ""

main :: IO ()
main = do
    printResult "mallocThenFree"     mallocThenFree
    printResult "sequential"         sequential
    printResult "missingFree"        missingFree
    printResult "freeWithoutMalloc"  freeWithoutMalloc
    printResult "doubleFree"         doubleFree
    printResult "wrongFree"          wrongFree
    printResult "reallocate"         reallocate
