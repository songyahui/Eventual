{-# OPTIONS_GHC -i../.. #-}
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
-- wNullable (evalFuture prog) gives the minimum total steps to fully
-- discharge all future obligations (∞ = no path to full discharge).

type TSRE = WRE Tropical

submit :: Int -> Pledge TSRE ()
submit taskId = Pledge
    { ret    = ()
    , pre    = WEps sone                                              -- no precondition
    , post   = WSingle (Tropical 1) (Atom "submit" (List [Num taskId]))
    , future = \_ -> wFinally (Tropical 1) (Atom "complete" (List [Num taskId]))
    }

complete :: Int -> Pledge TSRE ()
complete taskId = Pledge
    { ret    = ()
      -- pre: submit must have been observed (costs 1 step to verify)
    , pre    = wPreviously (Tropical 1) (Atom "submit" (List [Num taskId]))
    , post   = WSingle (Tropical 1) (Atom "complete" (List [Num taskId]))
    , future = \_ -> WEps sone                                        -- fully discharged
    }

-- abort is more expensive than complete (2 steps instead of 1).
abort :: Int -> Pledge TSRE ()
abort taskId = Pledge
    { ret    = ()
    , pre    = wPreviously (Tropical 1) (Atom "submit" (List [Num taskId]))
    , post   = WSingle (Tropical 2) (Atom "abort" (List [Num taskId]))
    , future = \_ -> WEps sone
    }

-- ── Programs ──────────────────────────────────────────────────────────────────

-- Good: submit then complete. Total future cost = 1 step (complete discharged).
submitAndComplete :: Pledge TSRE ()
submitAndComplete = do
    submit 1
    complete 1

-- Good: submit then abort. Future cost = 2 steps (abort is costlier).
submitAndAbort :: Pledge TSRE ()
submitAndAbort = do
    submit 1
    abort 1

-- Good: two tasks submitted and completed in sequence.
twoTasks :: Pledge TSRE ()
twoTasks = do
    submit 1
    complete 1
    submit 2
    complete 2

-- Bad: submit without completing — future = F[1](complete(1)), wNullable = ∞.
submitOnly :: Pledge TSRE ()
submitOnly = submit 1

-- Good: choose cheapest of complete vs abort using WAdd (⊕ = min).
-- This models a non-deterministic choice between two resolution strategies.
-- The future picks the minimum-cost option.
cheapestResolution :: Pledge TSRE ()
cheapestResolution = Pledge
    { ret    = ()
    , pre    = WEps sone
    , post   = WSingle (Tropical 1) (Atom "submit" (List [Num 1]))
      -- future: either complete (cost 1) or abort (cost 2) — min = 1
    , future = \_ -> WAdd
                  (wFinally (Tropical 1) (Atom "complete" (List [Num 1])))
                  (wFinally (Tropical 2) (Atom "abort"    (List [Num 1])))
    }

-- ── Display ───────────────────────────────────────────────────────────────────

printResult :: String -> Pledge TSRE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    let fut = wNormalize (evalFuture prog)
    putStrLn $ "Pre:           " ++ show (wNormalize (pre prog))
    putStrLn $ "Post:          " ++ show (wNormalize (post prog))
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
