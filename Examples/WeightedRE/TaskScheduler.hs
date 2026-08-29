module Examples.WeightedRE.TaskScheduler where

import Prelude hiding ((<>))
import Pledge
import Pledge.Semiring
import Pledge.WeightedRE

-- ── Model ─────────────────────────────────────────────────────────────────────
-- Minimum-cost task scheduling using the Tropical (min-plus) semiring.
-- Weights represent the minimum number of steps required to discharge an
-- obligation.
--
--   post of submit   = costs 1 step (the submit event itself).
--   future of submit = costs 1 more step to reach a complete event.
--   post of complete = costs 1 step.
--   post of abort    = costs 2 steps (more expensive cleanup).
--
-- smul = + accumulates costs along a sequential path.
-- sadd = min chooses the cheaper of two alternatives.
-- wNullable (future prog) gives the minimum total steps to fully
-- discharge all future obligations (∞ = no path to full discharge).

type TSRE = WRE Tropical Term

submit :: Int -> Pledge IO TSRE ()
submit taskId = Pledge $ return
    ( ()
    , wTop                                                                   -- pre: no precondition
    , WSingle (Tropical 1) (Atom "submit" (List [Num taskId]))               -- post
      -- future: the task must be resolved, by completing (1 step) or
      -- aborting (2 steps).  ⊕ = min, so the residual cost is that of the
      -- cheaper route still available.  Naming only `complete` here would
      -- make `submitAndAbort` report an undischarged obligation.
    , WAdd (wFinally (Tropical 1) (Atom "complete" (List [Num taskId])))
           (wFinally (Tropical 2) (Atom "abort"    (List [Num taskId])))
    )

complete :: Int -> Pledge IO TSRE ()
complete taskId = Pledge $ return
    ( ()
      -- pre: submit must have been observed (costs 1 step to verify)
    , wPreviously (Tropical 1) (Atom "submit" (List [Num taskId]))
    , WSingle (Tropical 1) (Atom "complete" (List [Num taskId]))             -- post
    , wTop                                                                   -- future: fully discharged
    )

-- abort is more expensive than complete (2 steps instead of 1).
abort :: Int -> Pledge IO TSRE ()
abort taskId = Pledge $ return
    ( ()
    , wPreviously (Tropical 1) (Atom "submit" (List [Num taskId]))
    , WSingle (Tropical 2) (Atom "abort" (List [Num taskId]))                -- post
    , wTop                                                                   -- future: fully discharged
    )

-- ── Programs ──────────────────────────────────────────────────────────────────

-- Good: submit then complete. Total future cost = 1 step (complete discharged).
submitAndComplete :: Pledge IO TSRE ()
submitAndComplete = do
    submit 1
    complete 1

-- Good: submit then abort. Future cost = 2 steps (abort is costlier).
submitAndAbort :: Pledge IO TSRE ()
submitAndAbort = do
    submit 1
    abort 1

-- Good: two tasks submitted and completed in sequence.
twoTasks :: Pledge IO TSRE ()
twoTasks = do
    submit 1
    complete 1
    submit 2
    complete 2

-- Bad: submit without completing — future = F[1](complete(1)), wNullable = ∞.
submitOnly :: Pledge IO TSRE ()
submitOnly = submit 1

-- Unresolved: a submit whose obligation is still open.  The residual is the
-- full disjunction, and wNullable is ∞ — not a violation, but a statement
-- that no trace of length zero discharges it.  Resolving it costs
-- min(1, 2) = 1 step, which is what `submitAndComplete` realises.
cheapestResolution :: Pledge IO TSRE ()
cheapestResolution = Pledge $ return
    ( ()
    , wTop     
    , WSingle (Tropical 1) (Atom "submit" (List [Num 1]))
      -- future: either complete (cost 1) or abort (cost 2) — min = 1
    , WAdd (wFinally (Tropical 1) (Atom "complete" (List [Num 1])))
           (wFinally (Tropical 2) (Atom "abort"    (List [Num 1])))
    )

-- ── Display ───────────────────────────────────────────────────────────────────

printResult :: String -> Pledge IO TSRE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    (_, preC, postC, futC) <- runPledge prog
    let fut = wNormalize futC
    putStrLn $ "Pre:           " ++ show (wNormalize preC)
    putStrLn $ "Post:          " ++ show (wNormalize postC)
    putStrLn $ "Future:        " ++ show fut
    putStrLn $ "Min cost:      " ++ show (wNullable fut)
    putStrLn ""

main :: IO ()
main = do
    printResult "submitAndComplete"    submitAndComplete
    printResult "submitAndAbort"       submitAndAbort
    printResult "twoTasks"             twoTasks
    printResult "submitOnly (bad)"     submitOnly
    printResult "cheapestResolution"   cheapestResolution
