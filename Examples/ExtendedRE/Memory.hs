{-# OPTIONS_GHC -i../.. #-}
module Examples.ExtendedRE.Memory where

import Prelude hiding ((<>))
import Pledge

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
-- ExtendedRE captures both simultaneously.

-- ── Primitives ────────────────────────────────────────────────────────────────

malloc :: Addr -> Pledge ExtendedRE Addr
malloc addr = Pledge
    { ret    = addr
    , pre    = fromRE universe                  -- no precondition
    , post   = fromRE (Single (Atom "malloc" (List [Num addr])))
      -- future: the cell must be live (h[addr] > 0) AND free must eventually happen
    , future = \a -> ExtendedRE
                        (PGt (ValAt a) (Lit 0))
                        (finally (Atom "free" (List [Num a])))
    }

free :: Addr -> Pledge ExtendedRE ()
free addr = Pledge
    { ret    = ()
      -- pre: cell must be live (heap side) AND malloc must have been observed (trace side)
    , pre    = ExtendedRE
                  (PGt (ValAt addr) (Lit 0))
                  (previously (Atom "malloc" (List [Num addr])))
    , post   = fromRE (Single (Atom "free" (List [Num addr])))
      -- future: no double-free until re-allocation
    , future = \_ -> fromRE (noUntil (Atom "free"   (List [Num addr]))
                                     (Atom "malloc" (List [Num addr])))
    }

-- ── Programs ──────────────────────────────────────────────────────────────────

-- Good: malloc then free — both constraints satisfied.
mallocThenFree :: Pledge ExtendedRE ()
mallocThenFree = do
    addr <- malloc 1
    free addr

-- Good: sequential pairs — each obligation discharged in turn.
sequential :: Pledge ExtendedRE ()
sequential = do
    a1 <- malloc 1
    free a1
    a2 <- malloc 2
    free a2

-- Bad: missing free — future carries finally(free(1)) ∧ h[1] > 0.
missingFree :: Pledge ExtendedRE ()
missingFree = do
    _ <- malloc 1
    return ()

-- Bad: free without malloc — pre contains previously(malloc(1)) which is not met.
freeWithoutMalloc :: Pledge ExtendedRE ()
freeWithoutMalloc = free 1

-- Bad: double-free — noUntil guard triggers on the second free.
doubleFree :: Pledge ExtendedRE ()
doubleFree = do
    addr <- malloc 1
    free addr
    free addr

-- Bad: free wrong address — future(malloc 1) = finally(free(1)) remains undischarged.
wrongFree :: Pledge ExtendedRE ()
wrongFree = do
    addr <- malloc 1
    free (addr + 1)

-- Good: reallocate after free — noUntil guard reset by the second malloc.
reallocate :: Pledge ExtendedRE ()
reallocate = do
    a <- malloc 1
    free a
    a' <- malloc 1
    free a'

-- ── Display ───────────────────────────────────────────────────────────────────

printResult :: forall a. Show a => String -> Pledge ExtendedRE a -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    let ExtendedRE prePred  preRE  = pre         prog
        ExtendedRE postPred postRE = post        prog
        ExtendedRE futPred  futRE  = evalFuture  prog
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
