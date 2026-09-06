{-# LANGUAGE ScopedTypeVariables #-}

-- | Concurrent runtime monitoring with 'Pledge.WeightedRE'.
--
-- Context
-- =======
-- A running system has several threads that share one lock.  Every thread is
-- supposed to follow the discipline
--
-- >   acquire(t) · use(t)* · release(t)
--
-- and critical sections must not overlap (mutual exclusion).  Each thread
-- writes its actions to one shared, totally-ordered event log (here a
-- 'TChan'); a dedicated /monitor/ thread consumes that interleaved global
-- stream online, one event at a time, and maintains a residual 'WRE' by
-- Brzozowski derivative.  Nothing about the program is known statically — the
-- monitor only sees the events as they happen.
--
-- Two signals come out of the 'WRE' at every step:
--
--   * __structural verdict__ — @residual == WBot@ means the observed prefix
--     can no longer be completed to a conformant trace: mutual exclusion has
--     been broken, or a @release@ appeared with no matching @acquire@.
--
--   * __quantitative verdict__ — weights live in the 'Prob' semiring, where
--     each event is only trusted to have been logged faithfully with
--     probability @0.98@ (lossy logging / flaky instrumentation).
--     @wNullable residual@ at a quiescent point is the product of those
--     per-event trusts, i.e. the confidence that the whole log seen so far is
--     a genuine, conformant execution rather than an artefact of dropped or
--     reordered records.
--
-- The specification is exactly what the 'Pledge' monad would accumulate as
-- the @post@ of @acquire t >> use t >> release t@ for each thread, starred and
-- unioned over the thread set — so the monitor checks the same contract the
-- library builds compositionally, just against a live trace.
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

-- | One complete, non-overlapping critical section by thread @t@:
-- @[.98]acquire(t) · ([.98]use(t))* · [.98]release(t)@.
session :: Int -> PRE
session t =
    WSeq (WSingle trust (ev "acquire" t))
   (WSeq (WStar (WSingle trust (ev "use" t)))
         (WSingle trust (ev "release" t)))

-- | Global lock discipline over a fixed thread set: the interleaved log must
-- be a sequence of complete critical sections that never overlap.
--
-- Because @session t@ only accepts @use(t)@ / @release(t)@ after @acquire(t)@,
-- an @acquire@ by any /other/ thread while the lock is held drives the
-- derivative straight to 'WBot'.
lockDiscipline :: [Int] -> PRE
lockDiscipline ts = WStar (foldr1 WAdd (map session ts))

-- ── Monitor ──────────────────────────────────────────────────────────────────

data Verdict
    = Quiescent Prob   -- ^ at a safe stopping point; confidence the log is genuine
    | InSection Prob   -- ^ mid critical section; product of trusts consumed so far
    | Broken           -- ^ residual is WBot: the discipline can no longer hold

verdict :: PRE -> Prob -> Verdict
verdict r confSoFar
    | rn == WBot        = Broken
    | w  /= szero        = Quiescent w
    | otherwise          = InSection confSoFar
  where
    rn = wNormalize r
    w  = wNullable rn

-- | Consume @n@ events from the shared log, folding the residual 'WRE' and a
-- running trust product.  This is the whole monitor: one derivative per event.
monitor :: [Int] -> TChan (Event Term) -> Int -> IO ()
monitor threads chan n = go n (lockDiscipline threads) sone
  where
    go 0 _ _ = putStrLn "    monitor: end of stream"
    go k r conf = do
        e <- atomically (readTChan chan)
        let r'    = wNormalize (wDerivative e r)
            conf' = smul conf trust
        putStrLn $ "    " ++ pad (show e) ++ report (verdict r' conf')
        go (k - 1) r' conf'

    report (Quiescent p) = "✓ safe point — trace-genuine confidence " ++ show p
    report (InSection p) = "… in critical section (confidence so far " ++ show p ++ ")"
    report Broken        = "✗ VIOLATION — mutual exclusion / protocol broken"

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
    monitor [1, 2] chan 6
    putStrLn ""

scenarioRacy :: IO ()
scenarioRacy = do
    putStrLn "── scenario B: two threads, no mutual exclusion ─────────────────"
    chan <- newTChanIO
    gate <- newEmptyMVar
    forM_ [1, 2] $ \tid -> forkIO (racyWorker chan gate tid)
    threadDelay 5000          -- let both acquires land
    putMVar gate ()
    monitor [1, 2] chan 6
    putStrLn ""

main :: IO ()
main = do
    putStrLn "WRE concurrent runtime monitor\n"
    scenarioSafe
    scenarioRacy
