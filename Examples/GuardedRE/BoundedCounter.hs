module Examples.GuardedRE.BoundedCounter where

import Prelude hiding ((<>))
import Control.Monad (replicateM_)
import Data.List (intercalate)
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
-- ⚠ THIS EXAMPLE IS OUTSIDE WHAT GuardedRE CAN EXPRESS, and is kept as a
-- cautionary case rather than a working one.  See Examples/GuardedRE/Memory.hs
-- for the instance used in the paper.
--
-- A GuardedRE pairs a trace RE with a *static* Presburger predicate: every
-- operation of the algebra conjoins the two predicates and quotients only the
-- two REs, so the predicate is never advanced by an event.  It can therefore
-- state an invariant of the whole run, but not a property that changes as the
-- run proceeds.
--
-- A counter is exactly the latter.  `initCounter` posts h[addr] = 0 and
-- `decrement` requires h[addr] > 0; these hold at *different moments*, but
-- composition conjoins them into h[addr] = 0 ∧ h[addr] > 0, which is
-- unsatisfiable.  The verdicts consequently invert:
--
--   normalUse  (correct)   ⇒ pre = [false]   -- reported as violating
--   overflow   (incorrect) ⇒ pre = [true]    -- h < 10 never contradicts h = 0
--
-- Expressing state change needs values indexed by trace position, i.e. a
-- Presburger encoding of the trace itself rather than a predicate alongside
-- it.  Neither constraint alone suffices for this problem, but neither does
-- their conjunction in this instance.

-- ── Primitives ────────────────────────────────────────────────────────────────

initCounter :: Addr -> Pledge IO (GuardedRE Term) ()
initCounter addr = Pledge $ return
    ( ()
    , fromRE universe                                   -- pre: no precondition
    , [ ( PEq (ValAt addr) (Lit 0)                      -- post: heap starts at zero
        , Single (Atom "init" (List [Num addr]))
        ) ]
    , fromRE universe                                   -- future: no obligation
    )

increment :: Addr -> Int -> Pledge IO (GuardedRE Term) ()
increment addr maxVal = Pledge $ return
    ( ()
      -- pre: must not already be at the maximum
    , [ (PLt (ValAt addr) (Lit maxVal), universe) ]
    , fromRE (Single (Atom "inc" (List [Num addr])))    -- post
      -- future: value stays non-negative
    , fromPPred (PGe (ValAt addr) (Lit 0))
    )

decrement :: Addr -> Pledge IO (GuardedRE Term) ()
decrement addr = Pledge $ return
    ( ()
      -- pre: must not already be at zero
    , [ (PGt (ValAt addr) (Lit 0), universe) ]
    , fromRE (Single (Atom "dec" (List [Num addr])))    -- post
      -- future: value stays non-negative
    , fromPPred (PGe (ValAt addr) (Lit 0))
    )

snapshot :: Addr -> Pledge IO (GuardedRE Term) ()
snapshot addr = Pledge $ return
    ( ()
    , fromRE universe                                   -- pre: no precondition
    , fromRE (Single (Atom "snapshot" (List [Num addr])))
    , fromRE universe                                   -- future: no obligation
    )

-- ── Programs ──────────────────────────────────────────────────────────────────

-- Intended good; reports pre = [false].  See the header note.
normalUse :: Pledge IO (GuardedRE Term) ()
normalUse = do
    initCounter 0
    increment   0 10
    increment   0 10
    decrement   0
    snapshot    0

-- Reports pre = [true]: no decrement, so no contradiction arises.
emptyRun :: Pledge IO (GuardedRE Term) ()
emptyRun = do
    initCounter 0
    snapshot    0

-- Intended good; reports pre = [false].  See the header note.
fillAndDrain :: Int -> Pledge IO (GuardedRE Term) ()
fillAndDrain maxVal = do
    initCounter 0
    replicateM_ maxVal (increment 0 maxVal)
    replicateM_ maxVal (decrement 0)
    snapshot    0

-- Intended bad; reports pre = [true], i.e. NOT caught.  See the header note.
-- The intent was that after 10 increments h[0] = 10 should violate the 11th
-- increment's PLt(h[0], 10).  But the predicate is static: conjoining
-- PLt(h[0],10) with itself eleven times is just PLt(h[0],10), which is
-- satisfiable, and nothing tracks that h[0] has grown.
overflow :: Pledge IO (GuardedRE Term) ()
overflow = do
    initCounter 0
    replicateM_ 11 (increment 0 10)
    snapshot    0

-- Intended bad; reports pre = [false], and so happens to be flagged -- but
-- for the wrong reason.  It is init's PEq(h[0],0) contradicting decrement's
-- PGt(h[0],0), the same conjunction that also condemns the correct
-- `normalUse` above, not a genuine underflow check.
underflow :: Pledge IO (GuardedRE Term) ()
underflow = do
    initCounter 0
    decrement   0

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
    putStrLn $ "Pre:    " ++ showGuarded preC
    putStrLn $ "Post:   " ++ showGuarded postC
    putStrLn $ "Future: " ++ showGuarded futC
    putStrLn ""
  where
    showGuarded gre = intercalate " ∨ "
        [ "[" ++ show p ++ "] " ++ show (normalize r) | (p, r) <- gre ]

main :: IO ()
main = do
    printResult "normalUse"        normalUse
    printResult "emptyRun"         emptyRun
    printResult "fillAndDrain 3"   (fillAndDrain 3)
    printResult "overflow (bad)"   overflow
    printResult "underflow (bad)"  underflow
    printResult "noInit (bad)"     noInit
