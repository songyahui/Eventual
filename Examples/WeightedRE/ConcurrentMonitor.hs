{-# LANGUAGE ScopedTypeVariables #-}

-- | Concurrent runtime monitoring with 'Pledge.WeightedRE', where the lock
-- discipline is expressed as a __future condition__ installed at the start of
-- each critical section.
--
-- Context
-- =======
-- Several threads share one lock.  Each thread emits @acquire(t)@, some
-- @use(t)@ events, then @release(t)@ onto a single shared, totally-ordered log
-- (a 'TChan').  A monitor thread consumes that interleaved stream online.
--
-- Instead of checking the trace against one global regular expression, the
-- monitor replays the log through the 'Pledge' bind rule for the
-- @(post, fut)@ pair:
--
-- >   post = postA · postB
-- >   fut  = (futA ∖ postB) ⊓ futB
--
-- (see @Pledge.Core@, the '>>=' definition).  Every event carries a @post@ (the
-- event itself) and a @fut@ (its future obligation).  Only one event installs a
-- non-trivial obligation:
--
-- >   acquire(t)   ⇒   fut = use(t)* · release(t) · Σ*
--
-- That single future condition, attached at session start, encodes the whole
-- discipline once the bind rule propagates it:
--
--   * __matched release__ — the obligation stays non-nullable until
--     @release(t)@ streams in and the left-quotient @fut ∖ release(t)@ reduces
--     it to @Σ*@ (discharged).
--
--   * __mutual exclusion / atomic section__ — while the obligation is open, the
--     only events it accepts as a prefix are @use(t)@ and @release(t)@.  Any
--     other event (another thread's @acquire@, or a stray @use@) makes
--     @fut ∖ event@ collapse to 'WBot', and @WBot ⊓ _ = WBot@ latches the
--     violation.
--
-- Weights live in the 'Prob' semiring: each event is trusted to have been
-- logged faithfully with probability 0.98.  @wNullable fut@ at a discharged
-- point is the product of the trusts of the @use@/@release@ events that
-- discharged the obligation — the confidence that the discharge is real and
-- not an artefact of lossy logging.
module Main (main) where

import Control.Concurrent       (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad            (forM_)
import Prelude hiding ((<>))
import Pledge
import Pledge.Semiring
import Pledge.WeightedRE

-- ── Specification ─────────────────────────────────────────────────────────────

type PRE = WRE Prob Term

-- | Probability that any one logged event is faithful.
trust :: Prob
trust = Prob 0.98

-- | A thread-tagged event, e.g. @acquire([1])@.
ev :: String -> Int -> Event Term
ev act tid = Atom act (List [Num tid])

-- | The future condition installed when thread @t@ enters its critical
-- section: the rest of the trace must continue as
-- @([.98]use(t))* · [.98]release(t)@ — and, because a 'WSeq' only admits a
-- prefix its left factor accepts, nothing but @use(t)@ / @release(t)@ may
-- appear before that @release@.  The trailing @Σ*@ (via @(⊤)@) is what lets a
-- discharged obligation reduce to the '(⊓)' identity instead of a dead @ε@.
sessionObligation :: Int -> PRE
sessionObligation t =
    WSeq (WStar (WSingle trust (ev "use" t)))
   (WSeq (WSingle trust (ev "release" t))
         (⊤))

-- | @(post, fut)@ annotation of a single observed event.  @acquire@ is the
-- only event that installs a real future obligation.
--
-- The @post@ is unit-weight: it is the /divisor/ in @futA ∖ postB@, and the
-- weighted quantity is the obligation being divided, not the observation
-- doing the dividing (this also matches what @wLeftQuotient@ is designed for).
annot :: Event Term -> (PRE, PRE)
annot e@(Atom "acquire" (List [Num t])) = (WSingle sone e, sessionObligation t)
annot e                                 = (WSingle sone e, (⊤))

-- | The 'Pledge' '>>=' rule, restricted to the @(post, fut)@ components.
-- @'(·)'@, @'(∖)'@ and @'(⊓)'@ are the 'Composable' operations of @WRE Prob@.
bindStep :: (PRE, PRE) -> (PRE, PRE) -> (PRE, PRE)
bindStep (postA, futA) (postB, futB) =
    ( postA · postB
    , (futA ∖ postB) ⊓ futB )

-- ── Monitor ──────────────────────────────────────────────────────────────────

-- | Replay @n@ events from the shared log through 'bindStep', reporting the
-- state of the accumulated future condition after each one.
monitor :: TChan (Event Term) -> Int -> IO ()
monitor chan n = go n (empty, (⊤))
  where
    go 0 (_, fut) = putStrLn $ "    ── end of stream: " ++ endReport fut
    go k st = do
        e <- atomically (readTChan chan)
        let st'@(_, fut') = bindStep st (annot e)
        putStrLn $ "    " ++ pad (show e) ++ stepReport fut'
        go (k - 1) st'

    stepReport fut
        | fut == WBot            = "✗ VIOLATION — future condition unsatisfiable"
        | wNullable fut /= szero  = "✓ obligation discharged (wNullable fut = "
                                        ++ show (wNullable fut) ++ ")"
        | otherwise              = "… critical section open — obligation pending"

    endReport fut
        | fut == WBot            = "✗ terminated in violation"
        | wNullable fut /= szero  = "✓ every session obligation discharged"
        | otherwise              = "✗ unfinished critical section (leaked lock)"

    pad s = s ++ replicate (max 1 (24 - length s)) ' '

-- ── Workers (real threads, real interleaving) ─────────────────────────────────

emit :: TChan (Event Term) -> String -> Int -> IO ()
emit chan act tid = do
    atomically (writeTChan chan (ev act tid))
    threadDelay 1000

-- | Correct worker: holds a real 'MVar' lock around its critical section, so
-- however the two threads interleave the log stays conformant.
safeWorker :: TChan (Event Term) -> MVar () -> Int -> IO ()
safeWorker chan lock tid = do
    () <- takeMVar lock
    emit chan "acquire" tid
    emit chan "use"     tid
    emit chan "release" tid
    putMVar lock ()

-- | Buggy worker: no lock at all.  @gate@ is opened only once both threads
-- have already emitted their @acquire@, forcing the overlapping-section bug
-- to show up deterministically.
racyWorker :: TChan (Event Term) -> MVar () -> Int -> IO ()
racyWorker chan gate tid = do
    emit chan "acquire" tid
    () <- readMVar gate
    emit chan "use"     tid
    emit chan "release" tid

-- ── Scenarios ────────────────────────────────────────────────────────────────

scenarioSafe :: IO ()
scenarioSafe = do
    putStrLn "── scenario A: two lock-respecting threads ──────────────────────"
    chan <- newTChanIO
    lock <- newMVar ()
    forM_ [1, 2] $ \tid -> forkIO (safeWorker chan lock tid)
    monitor chan 6
    putStrLn ""

scenarioRacy :: IO ()
scenarioRacy = do
    putStrLn "── scenario B: two threads, no mutual exclusion ─────────────────"
    chan <- newTChanIO
    gate <- newEmptyMVar
    forM_ [1, 2] $ \tid -> forkIO (racyWorker chan gate tid)
    threadDelay 5000          -- let both acquires land
    putMVar gate ()
    monitor chan 6
    putStrLn ""

main :: IO ()
main = do
    putStrLn "WRE monitor — lock discipline as a future condition\n"
    scenarioSafe
    scenarioRacy
