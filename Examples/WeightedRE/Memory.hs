{-# OPTIONS_GHC -i../.. #-}
module Examples.WeightedRE.Memory where

import Prelude hiding ((<>))
import Pledge
import Pledge.Semiring
import Pledge.WeightedRE

-- ── Model ─────────────────────────────────────────────────────────────────────
-- Probabilistic memory management: weights represent the probability that an
-- obligation is correctly met.
--
--   post of malloc = 99% chance the malloc event is faithfully recorded.
--   future of malloc = 95% chance free will eventually be called correctly.
--   pre  of free   = 95% confidence that a preceding malloc was observed.
--   post of free   = 99% chance the free event is faithfully recorded.
--
-- When programs compose via >>=, probabilities multiply (smul = *), so the
-- end-to-end reliability of a program is the product of its steps' weights.
-- wNullable (evalFuture prog) gives the probability that all future
-- obligations are already discharged (no more events needed).

type PRE  = WRE Prob   -- weighted RE over the probability semiring

malloc :: Addr -> Pledge PRE Addr
malloc addr = Pledge
    { ret    = addr
    , pre    = WEps sone                                    -- always allowed
    , post   = WSingle (Prob 0.99) (Atom "malloc" (List [Num addr]))
    , future = \a -> wFinally (Prob 0.95) (Atom "free" (List [Num a]))
    }

free :: Addr -> Pledge PRE ()
free addr = Pledge
    { ret    = ()
      -- pre: 95% confidence that malloc(addr) was previously observed
    , pre    = wPreviously (Prob 0.95) (Atom "malloc" (List [Num addr]))
    , post   = WSingle (Prob 0.99) (Atom "free" (List [Num addr]))
    , future = \_ -> WEps sone                              -- no further obligation
    }

-- ── Programs ──────────────────────────────────────────────────────────────────

-- Good: malloc then free. End-to-end probability = 0.95 (free obligation met).
mallocThenFree :: Pledge PRE ()
mallocThenFree = do
    addr <- malloc 1
    free addr

-- Good: two sequential malloc/free pairs.
sequential :: Pledge PRE ()
sequential = do
    a1 <- malloc 1
    free a1
    a2 <- malloc 2
    free a2

-- Bad: missing free — future carries wFinally (Prob 0.95) free(1).
-- wNullable of future = 0  (free obligation not discharged).
missingFree :: Pledge PRE ()
missingFree = do
    _ <- malloc 1
    return ()

-- Bad: free without malloc — pre carries wPreviously (Prob 0.95) malloc(1)
-- but no malloc was emitted.
freeWithoutMalloc :: Pledge PRE ()
freeWithoutMalloc = free 1

-- Bad: free the wrong address — future of malloc(1) = F[0.95](free(1)) remains.
wrongFree :: Pledge PRE ()
wrongFree = do
    addr <- malloc 1
    free (addr + 1)

-- ── Display ───────────────────────────────────────────────────────────────────

printResult :: forall a. Show a => String -> Pledge PRE a -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    let fut = wNormalize (evalFuture prog)
    putStrLn $ "Pre:             " ++ show (wNormalize (pre prog))
    putStrLn $ "Post:            " ++ show (wNormalize (post prog))
    putStrLn $ "Future:          " ++ show fut
    putStrLn $ "Future weight:   " ++ show (wNullable fut)
    putStrLn ""

main :: IO ()
main = do
    printResult "mallocThenFree"    mallocThenFree
    printResult "sequential"        sequential
    printResult "missingFree"       missingFree
    printResult "freeWithoutMalloc" freeWithoutMalloc
    printResult "wrongFree"         wrongFree
