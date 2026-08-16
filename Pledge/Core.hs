module Pledge.Core
    ( -- * Composable class
      Composable(..)
    , (·)
    , (/\)
    , (\\)
      -- * Pledge monad
    , Pledge(..)
    , liftPledge
      -- * Safe inspection (run once)
    , PledgeResult(..)
    , inspect
      -- * Component accessors (pure \/ effect-free @m@ only)
    , getRet
    , getPre
    , getPost
    , getFut
    ) where

class Composable a where
    concatenation :: a -> a -> a
    conjunction   :: a -> a -> a
    empty         :: a
    universe      :: a
    subtraction   :: a -> a -> a

-- Lift all Composable operations through any Applicative m.
-- This lets (·), (/\), (\\) work on m eff values directly.
instance {-# OVERLAPPABLE #-} (Composable eff, Applicative m) => Composable (m eff) where
    concatenation = liftA2 concatenation
    conjunction   = liftA2 conjunction
    subtraction   = liftA2 subtraction
    empty         = pure empty
    universe      = pure universe

infixl 6 ·
(·) :: Composable a => a -> a -> a
(·) = concatenation

infixl 7 /\
(/\) :: Composable a => a -> a -> a
(/\) = conjunction

infixl 5 \\
(\\) :: Composable a => a -> a -> a
a \\ b = subtraction b a

-- ── Pledge monad ─────────────────────────────────────────────────────────────
-- A single m-action that produces the return value together with its pre-,
-- post-, and future-conditions all at once.  Because all four components come
-- from one run of the m action, stateful resources (e.g. file handles) are
-- used exactly once per invocation.

newtype Pledge m eff a = Pledge { runPledge :: m (a, eff, eff, eff) }
--                                                ^   ^    ^    ^
--                                               ret pre  post future

-- | Embed a plain @m@ action into 'Pledge' with trivial temporal conditions:
-- precondition @universe@ (trivially satisfied), postcondition @empty@
-- (emits nothing), future condition @universe@ (no future obligation).
liftPledge :: (Composable eff, Applicative m) => m a -> Pledge m eff a
liftPledge ma = Pledge $ fmap (, universe, empty, universe) ma

-- | All four components of a completed 'Pledge' action, collected in one run.
data PledgeResult eff a = PledgeResult
    { pledgeReturn :: a    -- ^ the return value
    , pledgePre    :: eff  -- ^ precondition (what must have held before)
    , pledgePost   :: eff  -- ^ postcondition (what this action emitted)
    , pledgeFut    :: eff  -- ^ future condition (what must still hold after)
    }

-- | Run a 'Pledge' action exactly once and collect all four components.
-- Prefer this over 'getRet' \/ 'getPre' \/ 'getPost' \/ 'getFut' whenever
-- @m@ has observable side effects (e.g. 'IO'), because each of those
-- helpers calls 'runPledge' separately.
inspect :: Functor m => Pledge m eff a -> m (PledgeResult eff a)
inspect (Pledge ma) =
    fmap (\(a, pre, post, fut) -> PledgeResult a pre post fut) ma

-- | Extract the return value.
-- /Warning/: calls 'runPledge' independently — use 'inspect' when @m@ is 'IO'.
getRet :: Functor m => Pledge m eff a -> m a
getRet = fmap (\(ret, _, _, _) -> ret) . runPledge

-- | Extract the precondition.
-- /Warning/: calls 'runPledge' independently — use 'inspect' when @m@ is 'IO'.
getPre :: Functor m => Pledge m eff a -> m eff
getPre = fmap (\(_, pre, _, _) -> pre) . runPledge

-- | Extract the postcondition.
-- /Warning/: calls 'runPledge' independently — use 'inspect' when @m@ is 'IO'.
getPost :: Functor m => Pledge m eff a -> m eff
getPost = fmap (\(_, _, post, _) -> post) . runPledge

-- | Extract the future condition.
-- /Warning/: calls 'runPledge' independently — use 'inspect' when @m@ is 'IO'.
getFut :: Functor m => Pledge m eff a -> m eff
getFut = fmap (\(_, _, _, fut) -> fut) . runPledge

instance Functor m => Functor (Pledge m eff) where
    fmap f (Pledge ma) =
        Pledge $ fmap (\(a, pre, post, fut) -> (f a, pre, post, fut)) ma

instance (Composable eff, Monad m) => Applicative (Pledge m eff) where
    pure x = Pledge $ pure (x, universe, empty, universe)
    Pledge mf <*> Pledge mx = Pledge $ do
        (f, preF, postF, futF) <- mf
        (x, preX, postX, futX) <- mx
        return (f x, preF /\ (preX \\ postF), postF · postX, (futF \\ postX) /\ futX)

instance (Composable eff, Monad m) => Monad (Pledge m eff) where
    return = pure
    Pledge ma >>= g = Pledge $ do
        (a, preA, postA, futA) <- ma
        (b, preB, postB, futB) <- runPledge (g a)
        return (b, preA /\ (preB \\ postA), postA · postB, (futA \\ postB) /\ futB)

-- ── Monad law proofs ──────────────────────────────────────────────────────────
--
-- Notation: write a Pledge as (ret, pre, post, fut).
--   pure x          = (x, universe, empty, universe)
--   (P,Q,F) >>= g   -- where g ret = (ret', P', Q', F')
--     = (ret', P /\ (P' \\ Q),  Q · Q',  (F \\ Q') /\ F')
--
-- The proofs require 'Composable' to satisfy these eight laws:
--
--   (C1)  empty · a       = a           left  identity of (·)
--   (C2)  a · empty       = a           right identity of (·)
--   (C3)  (a · b) · c     = a · (b · c) associativity  of (·)
--   (C4)  universe /\ a   = a           universe is identity of (/\)
--   (C5)  a \\ empty      = a           empty post discharges nothing
--   (C6)  universe \\ a   = universe    universe is stable under subtraction
--   (C7)  x \\ (a · b)    = (x \\ b) \\ a   sequential residual
--   (C8)  (a /\ b) \\ c   = (a \\ c) /\ (b \\ c)  subtraction distributes over (/\)
--
-- ── Law 1: left identity — pure a >>= f = f a ─────────────────────────────────
--
-- pure a = (a, universe, empty, universe).
-- Let f a = (b, P', Q', F').  Then pure a >>= f gives:
--
--   pre  : universe /\ (P' \\ empty)
--        = universe /\ P'             -- by C5: P' \\ empty = P'
--        = P'                         -- by C4: universe /\ P' = P'
--
--   post : empty · Q'
--        = Q'                         -- by C1
--
--   fut  : (universe \\ Q') /\ F'
--        = universe /\ F'             -- by C6: universe \\ Q' = universe
--        = F'                         -- by C4
--
-- All three components equal those of f a.                                    □
--
-- ── Law 2: right identity — m >>= pure = m ────────────────────────────────────
--
-- Let m = (a, P, Q, F).  pure a = (a, universe, empty, universe).
-- m >>= pure gives:
--
--   pre  : P /\ (universe \\ Q)
--        = P /\ universe              -- by C6
--        = P                          -- by C4
--
--   post : Q · empty
--        = Q                          -- by C2
--
--   fut  : (F \\ empty) /\ universe
--        = F /\ universe              -- by C5
--        = F                          -- by C4
--
-- All three components equal those of m.                                       □
--
-- ── Law 3: associativity — (m >>= f) >>= g = m >>= (f >=> g) ─────────────────
--
-- Let m = (a,P,Q,F),  f a = (b,P',Q',F'),  g b = (c,P'',Q'',F'').
--
-- LHS: compute m >>= f first → (b, P∧(P'\\Q), Q·Q', (F\\Q')∧F'),
--      then bind g:
--
--   pre_L  = [P /\ (P' \\ Q)] /\ (P'' \\ (Q · Q'))
--   post_L = (Q · Q') · Q''
--   fut_L  = [((F \\ Q') /\ F') \\ Q''] /\ F''
--
-- RHS: compute f a >>= g first → (c, P'∧(P''\\Q'), Q'·Q'', (F'\\Q'')∧F''),
--      then bind that into m:
--
--   pre_R  = P /\ ((P' /\ (P'' \\ Q')) \\ Q)
--   post_R = Q · (Q' · Q'')
--   fut_R  = (F \\ (Q' · Q'')) /\ ((F' \\ Q'') /\ F'')
--
-- post: post_L = (Q · Q') · Q'' = Q · (Q' · Q'') = post_R              by C3  □
--
-- pre:  expand pre_R using C8 then C7:
--   (P' /\ (P'' \\ Q')) \\ Q
--     = (P' \\ Q) /\ ((P'' \\ Q') \\ Q)    by C8
--     = (P' \\ Q) /\ (P'' \\ (Q · Q'))     by C7
--   so pre_R = P /\ (P' \\ Q) /\ (P'' \\ (Q · Q')) = pre_L             by C3  □
--
-- fut:  expand fut_L using C8 then C7:
--   ((F \\ Q') /\ F') \\ Q''
--     = (F \\ Q') \\ Q''  /\  (F' \\ Q'')  by C8
--     = F \\ (Q' · Q'')   /\  (F' \\ Q'')  by C7
--   so fut_L = (F \\ (Q'·Q'')) /\ (F' \\ Q'') /\ F'' = fut_R           by C4  □
