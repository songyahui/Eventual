module Examples.GuardedRE.Memory where

import Prelude hiding ((<>))
import Data.List (intercalate)
import Pledge
import Pledge.GuardedRE
import System.Random

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

malloc :: Pledge IO (GuardedRE Term) Addr
malloc = Pledge $ do
    addr <- randomRIO (0, 5)
    return ( addr
           , fromRE universe                                     -- pre: no precondition
           , [ (PEq (ValAt addr) (Lit 0), Epsilon)
             , (PGt (ValAt addr) (Lit 0), Single (Atom "malloc" (List [Num addr])))
             ]
             -- future: cell must be live AND free must eventually happen
           , [ (PEq (ValAt addr) (Lit 0), never (Usage (List [Num addr])))
             , (PGt (ValAt addr) (Lit 0), finally (Atom "free" (List [Num addr])))
             ]
           )

free :: Addr -> Pledge IO (GuardedRE Term) ()
free addr = Pledge $ return
    ( ()
      -- pre: cell must be live (heap) AND malloc must have been observed (trace)
    , [ (PEq (ValAt addr) (Lit 0), universe)
      , (PGt (ValAt addr) (Lit 0), previously (Atom "malloc" (List [Num addr])))
      ]
    , fromRE (Single (Atom "free" (List [Num addr])))     -- post
      -- future: no double-free until re-allocation
    , fromRE (noUntil (Atom "free"   (List [Num addr]))
                      (Atom "malloc" (List [Num addr])))
    )

-- ── Programs ──────────────────────────────────────────────────────────────────

-- Good: malloc then free — both constraints satisfied.
mallocThenFree :: Pledge IO (GuardedRE Term) ()
mallocThenFree = do
    addr <- malloc
    free addr

-- Good: sequential pairs — each obligation discharged in turn.
sequential :: Pledge IO (GuardedRE Term) ()
sequential = do
    a1 <- malloc
    free a1
    a2 <- malloc
    free a2

-- Bad: missing free — future carries finally(free(1)) ∧ h[1] > 0.
missingFree :: Pledge IO (GuardedRE Term) ()
missingFree = do
    _ <- malloc
    return ()

-- Bad: free without malloc — pre contains previously(malloc(1)) which is not met.
freeWithoutMalloc :: Pledge IO (GuardedRE Term) ()
freeWithoutMalloc = free 1

-- Bad: double-free — noUntil guard triggers on the second free.
doubleFree :: Pledge IO (GuardedRE Term) ()
doubleFree = do
    addr <- malloc
    free addr
    free addr

-- Bad: free wrong address — future(malloc 1) = finally(free(1)) remains undischarged.
wrongFree :: Pledge IO (GuardedRE Term) ()
wrongFree = do
    addr <- malloc
    free (addr + 1)

-- Good: reallocate after free — noUntil guard reset by the second malloc.
reallocate :: Pledge IO (GuardedRE Term) ()
reallocate = do
    a <- malloc
    free a
    a' <- malloc
    free a'

-- ── Display ───────────────────────────────────────────────────────────────────

printResult :: forall a. Show a => String -> Pledge IO (GuardedRE Term) a -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    (_, preC, postC, futC) <- runPledge prog
    preC' <- normalizeGuardedSMT preC
    postC' <- normalizeGuardedSMT postC
    futC' <- normalizeGuardedSMT futC
    putStrLn $ "Pre:    " ++ showGuarded preC'
    putStrLn $ "Post:   " ++ showGuarded postC'
    putStrLn $ "Future: " ++ showGuarded futC'
    putStrLn ""
  where
    showGuarded gre = intercalate " \n ∨ "
        [ "[" ++ show p ++ "] " ++ show r | (p, r) <- gre ]

main :: IO ()
main = do
    printResult "mallocThenFree"     mallocThenFree
    printResult "sequential"         sequential
    printResult "missingFree"        missingFree
    printResult "freeWithoutMalloc"  freeWithoutMalloc
    printResult "doubleFree"         doubleFree
    printResult "wrongFree"          wrongFree
    printResult "reallocate"         reallocate
