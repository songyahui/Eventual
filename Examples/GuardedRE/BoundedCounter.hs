{-# OPTIONS_GHC -i../.. #-}
module Examples.GuardedRE.BoundedCounter where

import Prelude hiding ((<>))
import Control.Monad (replicateM_)
import Pledge
import Pledge.GuardedRE

-- ── Model ─────────────────────────────────────────────────────────────────────
-- A counter stored at heap address `addr` with a compile-time maximum `maxVal`.
--
--   Trace constraint (RE):   operations must follow the protocol
--                              init · (inc | dec)* · snapshot
--   Heap constraint (PPred): the counter value satisfies 0 ≤ h[addr] ≤ maxVal
--                            at every step; inc/dec are guarded accordingly.
--
-- Neither constraint alone is sufficient:
--   RE alone would accept inc-sequences that overflow the counter.
--   PPred alone would accept arbitrary event sequences.

-- ── Primitives ────────────────────────────────────────────────────────────────

initCounter :: Addr -> Pledge IO (GuardedRE Term) ()
initCounter addr = Pledge $ return
    ( ()
    , fromRE universe                                   -- pre: no precondition
    , GuardedRE (PEq (ValAt addr) (Lit 0))              -- post: heap starts at zero
                (Single (Atom "init" (List [Num addr])))
    , fromRE universe                                   -- future: no obligation
    )

increment :: Addr -> Int -> Pledge IO (GuardedRE Term) ()
increment addr maxVal = Pledge $ return
    ( ()
      -- pre: must not already be at the maximum
    , GuardedRE (PLt (ValAt addr) (Lit maxVal)) universe
    , fromRE (Single (Atom "inc" (List [Num addr])))    -- post
      -- future: value stays non-negative
    , fromPPred (PGe (ValAt addr) (Lit 0))
    )

decrement :: Addr -> Pledge IO (GuardedRE Term) ()
decrement addr = Pledge $ return
    ( ()
      -- pre: must not already be at zero
    , GuardedRE (PGt (ValAt addr) (Lit 0)) universe
    , fromRE (Single (Atom "dec" (List [Num addr])))    -- post
      -- future: value stays non-negative
    , fromPPred (PGe (ValAt addr) (Lit 0))
    )

snapshot :: Addr -> Pledge IO (GuardedRE Term) ()
snapshot addr = Pledge $ return
    ( ()
    , fromRE universe                                   -- pre: no precondition
    , GuardedRE PTrue (Single (Atom "snapshot" (List [Num addr])))
    , fromRE universe                                   -- future: no obligation
    )

-- ── Programs ──────────────────────────────────────────────────────────────────

-- Good: init, two increments, one decrement, snapshot.
normalUse :: Pledge IO (GuardedRE Term) ()
normalUse = do
    initCounter 0
    increment   0 10
    increment   0 10
    decrement   0
    snapshot    0

-- Good: init and immediately snapshot (counter stays at zero).
emptyRun :: Pledge IO (GuardedRE Term) ()
emptyRun = do
    initCounter 0
    snapshot    0

-- Good: fill to max, then drain to zero.
fillAndDrain :: Int -> Pledge IO (GuardedRE Term) ()
fillAndDrain maxVal = do
    initCounter 0
    replicateM_ maxVal (increment 0 maxVal)
    replicateM_ maxVal (decrement 0)
    snapshot    0

-- Bad: increment past maximum — pre of increment carries PLt(h[0], 10)
--      but after 10 increments h[0] = 10, so the 11th pre is violated.
overflow :: Pledge IO (GuardedRE Term) ()
overflow = do
    initCounter 0
    replicateM_ 11 (increment 0 10)  -- 11th violates PLt
    snapshot    0

-- Bad: decrement below zero — pre of decrement carries PGt(h[0], 0)
--      but after init the counter is 0, so the first decrement is rejected.
underflow :: Pledge IO (GuardedRE Term) ()
underflow = do
    initCounter 0
    decrement   0   -- pre requires h[0] > 0, but h[0] = 0

-- Bad: skip init — trace pre of initCounter not satisfied.
noInit :: Pledge IO (GuardedRE Term) ()
noInit = do
    increment 0 10
    snapshot  0

-- ── Display ───────────────────────────────────────────────────────────────────

printResult :: String -> Pledge IO (GuardedRE Term) () -> IO ()
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
    printResult "normalUse"        normalUse
    printResult "emptyRun"         emptyRun
    printResult "fillAndDrain 3"   (fillAndDrain 3)
    printResult "overflow (bad)"   overflow
    printResult "underflow (bad)"  underflow
    printResult "noInit (bad)"     noInit
