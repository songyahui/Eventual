module Examples.RE.MemorySpec where

import Prelude hiding ((<>))
import Pledge
import System.Random

-- ── PledgeSpec: the pure half ─────────────────────────────────────────────────
--
-- All four components of a Pledge, but with NO `m` wrapper.
-- This is a plain Haskell value: it can be constructed, inspected,
-- composed, and tested without running any IO.

data PledgeSpec eff a = PledgeSpec
    { specRet  :: a    -- return value (known statically)
    , specPre  :: eff  -- precondition
    , specPost :: eff  -- postcondition
    , specFut  :: eff  -- future obligation
    }

-- | Combine a pure spec with a real implementation.
-- The spec-builder receives the actual return value produced by `m`,
-- allowing data-dependent specs (e.g. malloc's address determines its future).
attach :: Functor m => (a -> PledgeSpec eff a) -> m a -> Pledge m eff a
attach mkSpec action = Pledge $ fmap go action
  where
    go a = let s = mkSpec a in (a, specPre s, specPost s, specFut s)

-- ── Pure specs (no IO) ────────────────────────────────────────────────────────
--
-- These are ordinary functions over Addr. They have no m, no IO, no randomness.
-- They can be called in pure code, QuickCheck properties, unit tests, etc.

type RETerm = RE Term

mallocSpec :: Addr -> PledgeSpec RETerm Addr
mallocSpec addr = PledgeSpec
    { specRet  = addr
    , specPre  = universe
    , specPost = Single (Atom "malloc" (List [Num addr]))
    , specFut  = finally (Atom "free" (List [Num addr]))
    }

freeSpec :: Addr -> PledgeSpec RETerm ()
freeSpec addr = PledgeSpec
    { specRet  = ()
    , specPre  = previously (Atom "malloc" (List [Num addr]))
    , specPost = Single (Atom "free" (List [Num addr]))
    , specFut  = noUntil (Atom "free" (List [Num addr])) (Atom "malloc" (List [Num addr]))
    }

-- ── IO implementations (no spec knowledge) ───────────────────────────────────

mallocImpl :: IO Addr
mallocImpl = randomRIO (1, 1000)

freeImpl :: Addr -> IO ()
freeImpl _ = return ()  -- real allocator would release memory here

-- ── Pledges: spec + impl joined at the boundary ───────────────────────────────

malloc' :: Pledge IO RETerm Addr
malloc' = attach mallocSpec mallocImpl

free' :: Addr -> Pledge IO RETerm ()
free' addr = attach (const (freeSpec addr)) (freeImpl addr)

-- ── Pure spec composition (the bind rule, lifted to PledgeSpec) ───────────────
--
-- Mirrors the Pledge monad's >>= rule exactly, but over plain values:
--
--   pre  (p >>= g) = pre p /\ (pre g \\ post p)
--   post (p >>= g) = post p · post g
--   fut  (p >>= g) = (fut p \\ post g) /\ fut g
--
-- Because everything is pure, this runs at compile time / in pure tests.

composePure :: Composable eff
            => PledgeSpec eff a
            -> (a -> PledgeSpec eff b)
            -> PledgeSpec eff b
composePure p mkQ =
    let q = mkQ (specRet p)
    in PledgeSpec
        { specRet  = specRet q
        , specPre  = specPre  p /\ (specPre  q \\ specPost p)
        , specPost = specPost p  ·   specPost q
        , specFut  = (specFut p \\ specPost q) /\ specFut q
        }

-- ── Static checks: pure Bool, zero IO ────────────────────────────────────────
--
-- These run entirely in pure Haskell. No `unsafePerformIO`, no test harness
-- that executes actions — just RE algebra on plain PledgeSpec values.
-- They are suitable for HUnit assertions or QuickCheck properties.

-- | (1) Pending obligation: malloc alone leaves a non-trivial future.
--   `finally(free(addr))` is not the universe — there IS a pending obligation.
--   Detects "missing free" structurally, before any IO runs.
checkMallocLeavesObligation :: Addr -> Bool
checkMallocLeavesObligation addr =
    normalize (specFut (mallocSpec addr)) /= top
  where
    top = Not Bot

-- | (2) Precondition: free requires a preceding malloc.
--   `nullable` tests whether the empty trace satisfies the RE.
--   `previously(malloc(addr))` = Σ* · malloc(addr) · Σ*, which does NOT
--   contain ε, so `nullable` returns False.
--   This catches `freeWithoutMalloc` statically.
checkFreeRequiresMalloc :: Addr -> Bool
checkFreeRequiresMalloc addr =
    not (nullable (specPre (freeSpec addr)))

-- | (3) Obligation discharged: malloc followed by free has no pending future.
--   After composing the two specs purely, the residual future is `noUntil`,
--   which IS the universe-level guard (no more `finally` pending).
--   Specifically: fut simplifies to noUntil(free(addr), malloc(addr)),
--   which is `Not Bot` only after a malloc resets it — not a dangling `finally`.
checkMallocFreeClearsFuture :: Addr -> Bool
checkMallocFreeClearsFuture addr =
    let combined = mallocSpec addr `composePure` \_ -> freeSpec addr
        fut = normalize (specFut combined)
    -- finally(free) is gone; only the double-free guard remains
    in fut /= finally (Atom "free" (List [Num addr]))

-- | (4) Double-free: composing malloc → free → free drives future to Bot (∅).
--   noUntil(free, malloc) says "free must not recur before malloc".
--   Taking the derivative of noUntil w.r.t. free gives Bot.
--   So after the second free, specFut = Bot /\ noUntil = Bot.
--   Bot means the trace is unsatisfiable — the violation is detected purely.
checkDoubleFreeFutureIsBot :: Addr -> Bool
checkDoubleFreeFutureIsBot addr =
    let afterMallocFree = mallocSpec addr `composePure` \_ -> freeSpec addr
        afterDoubleFree = afterMallocFree  `composePure` \_ -> freeSpec addr
    in normalize (specFut afterDoubleFree) == Bot

-- | (5) Missing free: two mallocs then one free leaves a dangling future.
--   The second malloc's `finally(free(addr2))` is never discharged.
--   Only meaningful when addr1 /= addr2; returns True vacuously otherwise.
checkMissingFreeLeavesObligation :: Addr -> Addr -> Bool
checkMissingFreeLeavesObligation addr1 addr2
    | addr1 == addr2 = True  -- same address: obligation aliases, not a useful test
    | otherwise =
        let step1 = mallocSpec addr1 `composePure` \_ -> mallocSpec addr2
            step2 = step1            `composePure` \_ -> freeSpec addr1
            fut   = normalize (specFut step2)
        in fut /= Not Bot   -- future is not universe — addr2 is still pending

-- ── Demonstration ─────────────────────────────────────────────────────────────

-- | Print the composed spec for malloc→free, then run the real IO action.
--   Notice: the spec is printed BEFORE any IO executes, because it is pure.
main :: IO ()
main = do
    let addr = 42   -- hypothetical address for static display

    putStrLn "=== Static spec for malloc(42) ==="
    putStrLn $ "Pre:    " ++ show (normalize (specPre  (mallocSpec addr)))
    putStrLn $ "Post:   " ++ show (normalize (specPost (mallocSpec addr)))
    putStrLn $ "Future: " ++ show (normalize (specFut  (mallocSpec addr)))

    putStrLn "\n=== Static spec for free(42) ==="
    putStrLn $ "Pre:    " ++ show (normalize (specPre  (freeSpec addr)))
    putStrLn $ "Post:   " ++ show (normalize (specPost (freeSpec addr)))
    putStrLn $ "Future: " ++ show (normalize (specFut  (freeSpec addr)))

    let combined = mallocSpec addr `composePure` \_ -> freeSpec addr
    putStrLn "\n=== Composed spec: malloc(42) >>= free(42) — computed PURELY ==="
    putStrLn $ "Pre:    " ++ show (normalize (specPre  combined))
    putStrLn $ "Post:   " ++ show (normalize (specPost combined))
    putStrLn $ "Future: " ++ show (normalize (specFut  combined))

    putStrLn "\n=== Static checks (all pure, no IO) ==="
    putStrLn $ "malloc leaves obligation:     " ++ show (checkMallocLeavesObligation addr)
    putStrLn $ "free requires malloc:         " ++ show (checkFreeRequiresMalloc addr)
    putStrLn $ "malloc>>free clears future:   " ++ show (checkMallocFreeClearsFuture addr)
    putStrLn $ "double-free => Bot future:    " ++ show (checkDoubleFreeFutureIsBot addr)
    putStrLn $ "two mallocs one free => leak: " ++ show (checkMissingFreeLeavesObligation addr (addr+1))

    putStrLn "\n=== Now running real IO (malloc' then free') ==="
    PledgeResult a pre post fut <- inspect (malloc' >>= free')
    putStrLn $ "Ret:    " ++ show a
    putStrLn $ "Pre:    " ++ show (normalize pre)
    putStrLn $ "Post:   " ++ show (normalize post)
    putStrLn $ "Future: " ++ show (normalize fut)
