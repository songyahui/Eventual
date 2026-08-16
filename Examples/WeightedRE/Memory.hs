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
-- wNullable (future prog) gives the probability that all future
-- obligations are already discharged (no more events needed).

type PRE = WRE Prob Term   -- weighted RE over the probability semiring

malloc :: Addr -> Pledge IO PRE Addr
malloc addr = Pledge $ return
    ( addr
    , WEps sone                                                         -- pre: always allowed
    , WSingle (Prob 0.99) (Atom "malloc" (List [Num addr]))             -- post
    , wFinally (Prob 0.95) (Atom "free" (List [Num addr]))              -- future
    )

free :: Addr -> Pledge IO PRE ()
free addr = Pledge $ return
    ( ()
      -- pre: 95% confidence that malloc(addr) was previously observed
    , wPreviously (Prob 0.95) (Atom "malloc" (List [Num addr]))
    , WSingle (Prob 0.99) (Atom "free" (List [Num addr]))               -- post
    , WEps sone                                                         -- future: no obligation
    )

-- ── Programs ──────────────────────────────────────────────────────────────────

-- Good: malloc then free. End-to-end probability = 0.95 (free obligation met).
mallocThenFree :: Pledge IO PRE ()
mallocThenFree = do
    addr <- malloc 1
    free addr

-- Good: two sequential malloc/free pairs.
sequential :: Pledge IO PRE ()
sequential = do
    a1 <- malloc 1
    free a1
    a2 <- malloc 2
    free a2

-- Bad: missing free — future carries wFinally (Prob 0.95) free(1).
-- wNullable of future = 0  (free obligation not discharged).
missingFree :: Pledge IO PRE ()
missingFree = do
    _ <- malloc 1
    return ()

-- Bad: free without malloc — pre carries wPreviously (Prob 0.95) malloc(1)
-- but no malloc was emitted.
freeWithoutMalloc :: Pledge IO PRE ()
freeWithoutMalloc = free 1

-- Bad: free the wrong address — future of malloc(1) = F[0.95](free(1)) remains.
wrongFree :: Pledge IO PRE ()
wrongFree = do
    addr <- malloc 1
    free (addr + 1)

-- ── Display ───────────────────────────────────────────────────────────────────

printResult :: forall a. Show a => String -> Pledge IO PRE a -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    (_, preC, postC, futC) <- runPledge prog
    let fut = wNormalize futC
    putStrLn $ "Pre:             " ++ show (wNormalize preC)
    putStrLn $ "Post:            " ++ show (wNormalize postC)
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
