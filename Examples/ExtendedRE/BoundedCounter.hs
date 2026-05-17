{-# OPTIONS_GHC -i../.. #-}
module Examples.ExtendedRE.BoundedCounter where

import Prelude hiding ((<>))
import Pledge

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

initCounter :: Addr -> Pledge ExtendedRE ()
initCounter addr = Pledge
    { ret    = ()
    , pre    = fromRE universe
    , post   = ExtendedRE
                  (PEq (ValAt addr) (Lit 0))        -- heap: starts at zero
                  (Single (Atom "init" (List [Num addr])))
    , future = \_ -> fromRE universe
    }

increment :: Addr -> Int -> Pledge ExtendedRE ()
increment addr maxVal = Pledge
    { ret    = ()
      -- pre: must not already be at the maximum
    , pre    = ExtendedRE
                  (PLt (ValAt addr) (Lit maxVal))
                  universe
    , post   = fromRE (Single (Atom "inc" (List [Num addr])))
      -- future: value stays non-negative after the increment
    , future = \_ -> fromPPred (PGe (ValAt addr) (Lit 0))
    }

decrement :: Addr -> Pledge ExtendedRE ()
decrement addr = Pledge
    { ret    = ()
      -- pre: must not already be at zero
    , pre    = ExtendedRE
                  (PGt (ValAt addr) (Lit 0))
                  universe
    , post   = fromRE (Single (Atom "dec" (List [Num addr])))
      -- future: value stays non-negative after the decrement
    , future = \_ -> fromPPred (PGe (ValAt addr) (Lit 0))
    }

snapshot :: Addr -> Pledge ExtendedRE ()
snapshot addr = Pledge
    { ret    = ()
    , pre    = fromRE universe
    , post   = ExtendedRE
                  PTrue
                  (Single (Atom "snapshot" (List [Num addr])))
    , future = \_ -> fromRE universe
    }

-- ── Programs ──────────────────────────────────────────────────────────────────

-- Good: init, two increments, one decrement, snapshot.
normalUse :: Pledge ExtendedRE ()
normalUse = do
    initCounter 0
    increment   0 10
    increment   0 10
    decrement   0
    snapshot    0

-- Good: init and immediately snapshot (counter stays at zero).
emptyRun :: Pledge ExtendedRE ()
emptyRun = do
    initCounter 0
    snapshot    0

-- Good: fill to max, then drain to zero.
fillAndDrain :: Int -> Pledge ExtendedRE ()
fillAndDrain maxVal = do
    initCounter 0
    foldr (>>) (return ()) (replicate maxVal (increment 0 maxVal))
    foldr (>>) (return ()) (replicate maxVal (decrement 0))
    snapshot    0

-- Bad: increment past maximum — pre of increment carries PLt(h[0], 10)
--      but after 10 increments h[0] = 10, so the 11th pre is violated.
overflow :: Pledge ExtendedRE ()
overflow = do
    initCounter 0
    foldr (>>) (return ()) (replicate 11 (increment 0 10))  -- 11th violates PLt
    snapshot    0

-- Bad: decrement below zero — pre of decrement carries PGt(h[0], 0)
--      but after init the counter is 0, so the first decrement is rejected.
underflow :: Pledge ExtendedRE ()
underflow = do
    initCounter 0
    decrement   0   -- pre requires h[0] > 0, but h[0] = 0

-- Bad: skip init — trace pre of initCounter not satisfied.
noInit :: Pledge ExtendedRE ()
noInit = do
    increment 0 10
    snapshot  0

-- ── Display ───────────────────────────────────────────────────────────────────

printResult :: String -> Pledge ExtendedRE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    let ExtendedRE prePred  preRE  = pre        prog
        ExtendedRE postPred postRE = post       prog
        ExtendedRE futPred  futRE  = evalFuture prog
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
