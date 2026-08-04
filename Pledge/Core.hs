{-# OPTIONS_GHC -i.. #-}
module Pledge.Core
    ( -- * Composable class
      Composable(..)
    , (·)
    , (/\)
    , (\\)
      -- * Pledge monad
    , Pledge(..)
    ) where

class Composable a where
    concatenation :: a -> a -> a
    conjunction   :: a -> a -> a
    empty         :: a
    universe      :: a
    subtraction   :: a -> a -> a

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

-- future is now indexed by the return value (direction 1: data-dependent
-- future conditions).  This lets an operation's temporal obligation refer
-- to whatever resource handle it returns, e.g.
--   mallocFresh addr = Effectful { ..., future = \a -> finally(free(a)) }
-- so that the exact address returned drives the obligation.

data Pledge eff a = Pledge
    { ret    :: a
    , pre    :: eff
    , post   :: eff
    , future :: eff
    }

instance Functor (Pledge eff) where
    -- fmap changes the return type from a to b, so future must become
    -- b -> eff.  We evaluate it at the known original return value and
    -- ignore the new b argument (the obligation is already determined).
    fmap f e = Pledge
        { ret    = f $ ret e
        , pre    = pre e
        , post   = post e
        , future = future e
        }

instance Composable eff => Applicative (Pledge eff) where
    pure x = Pledge
        { ret    = x
        , pre    = universe
        , post   = empty
        , future = universe
        }
    f <*> x = Pledge
        { ret    = ret f $ ret x
        -- Traditional precondition: pre of f, plus the residual of pre x
        -- not covered by post f.  Mirrors the sequential Hoare rule:
        --   {P} f {Q},  {P'} x {R}  ⊢  {P /\ (P' \\ Q)} f <*> x {R}
        , pre    = pre f /\ (pre x \\ post f)
        , post   = post f · post x
        -- Future obligation: what f still requires in the future after post x
        -- covers some of it, conjoined with x's own future obligation.
        --   future(f <*> x) = (futureF \\ postX) /\ futureX
        , future = (future f \\ post x) /\ future x
        }


instance Composable eff => Monad (Pledge eff) where
    return = pure
    e >>= f =
        let fe = f $ ret e in
        Pledge
        { ret    = ret fe
        -- Traditional precondition: pre of e, plus the residual of pre fe
        -- not covered by post e.  Mirrors the sequential Hoare rule:
        --   {P} e {Q},  {P'} fe {R}  ⊢  {P /\ (P' \\ Q)} e >>= f {R}
        , pre    = pre e /\ (pre fe \\ post e)
        , post   = post e · post fe
        -- Future obligation: what e still requires in the future after post fe
        -- covers some of it, conjoined with fe's own future obligation.
        --   future(e >>= f) = (futureE \\ postFE) /\ futureFE
        , future = (future e \\ post fe) /\ future fe
        }

-- To be a lawful Monad, Pledge must satisfy the three monad laws.  The following
-- table shows the pre, post, and future obligations of both sides of each law,
-- and the conditions under which they are equal.  The laws of the Composable
-- class are sufficient to guarantee equality of the ret and post fields, but
-- additional laws are required for the pre and future fields.  These are listed
-- below each table.
-- Law 1: Left identity — return a >>= f = f a
--
--   return a has pre = universe, post = const empty, future = const universe. So:
--
--   ┌────────┬──────────────────────────┬────────────┬───────────────────────┐
--   │ Field  │      return a >>= f      │    f a     │       Equal if…       │
--   ├────────┼──────────────────────────┼────────────┼───────────────────────┤
--   │ ret    │ ret (f a)                │ ret (f a)  │ ✓ always              │
--   ├────────┼──────────────────────────┼────────────┼───────────────────────┤
--   │ pre    │ universe /\ (pre (f a)   │ pre (f a)  │ x \\ empty = x and    │
--   │        │ \\ empty)                │            │ universe /\ x = x     │
--   ├────────┼──────────────────────────┼────────────┼───────────────────────┤
--   │ post   │ const (empty · postFA)   │ const      │ empty is left         │
--   │        │                          │ postFA     │ identity for ·        │
--   ├────────┼──────────────────────────┼────────────┼───────────────────────┤
--   │ future │ const ((universe \\      │ const      │ universe \\ x =       │
--   │        │ postFA) /\ futureFA)     │ futureFA   │ universe              │
--   └────────┴──────────────────────────┴────────────┴───────────────────────┘
--
-- So we need:
--     - x \\ empty = x
--     - universe /\ x = x
--     - empty · x = x
--     - universe \\ x = universe
--
-- Law 2: Right identity — m >>= return = m
--
--   return (ret m) has pre = universe, post = const empty, future = const
--   universe. So:
--
--   ┌────────┬────────────────────────┬───────────┬───────────────────────────┐
--   │ Field  │      m >>= return      │     m     │         Equal if…         │
--   ├────────┼────────────────────────┼───────────┼───────────────────────────┤
--   │ ret    │ ret m                  │ ret m     │ ✓ always                  │
--   ├────────┼────────────────────────┼───────────┼───────────────────────────┤
--   │ pre    │ pre m /\ (universe \\  │ pre m     │ universe \\ x = universe  │
--   │        │ postM)                 │           │ (same issue as above)     │
--   ├────────┼────────────────────────┼───────────┼───────────────────────────┤
--   │ post   │ const (postM · empty)  │ const     │ empty is right identity   │
--   │        │                        │ postM     │ for ·                     │
--   ├────────┼────────────────────────┼───────────┼───────────────────────────┤
--   │ future │ const ((futureM \\     │ const     │ x \\ empty = x and x /\   │
--   │        │ empty) /\ universe)    │ futureM   │ universe = x              │
--   └────────┴────────────────────────┴───────────┴───────────────────────────┘
--
-- So we need:
--     - x \\ empty = x
--     - universe /\ x = x
--     - x · empty = x
--     - universe \\ x = universe
--
-- Law 3: Associativity — (m >>= f) >>= g = m >>= (\x -> f x >>= g)
--
--   Working out both sides (with fm = f (ret m), gm
--   = g (ret fm)):
--
--   post (both sides):
--   - LHS: (postM · postFM) · postGM
--   - RHS: postM · (postFM · postGM)
--
--   So we need · to be associative.
--
--   pre (both sides):
--   - LHS: (pre m /\ (pre fm \\ postM)) /\ (pre gm \\ (postM · postFM))
--   - RHS: pre m /\ ((pre fm /\ (pre gm \\ postFM)) \\ postM)
--
--   For equality, you need (pre gm \\ (postM · postFM)) = (pre gm \\ postFM) \\
--   postM, i.e.:
--
--   x \\ (a · b)  =  (x \\ b) \\ a
--
--   This is a non-trivial distributivity law — subtraction must distribute over
--   concatenation in this specific way. It holds in trace-based models
--   (prefix-closed residuals) but is not guaranteed by the Composable class.
--
--   future (both sides):
--   - LHS: (((futureM \\ postFM) /\ futureFM) \\ postGM) /\ futureGM
--   - RHS: (futureM \\ ((futureFM \\ postGM) /\ futureGM)) /\ (futureFM \\ postGM)
--    /\ futureGM
--
--   We need laws like:
--
--   (a /\ b) \\ c = (a \\ c) /\ (b \\ c)  (subtraction distributes over
--   conjunction)
--   a \\ (b /\ c) = (a \\ b) \/ (a \\ c)  (or some variant)

-- So in summary, the following laws are required for Pledge to be a lawful Monad:
-- | Algebraic structure underlying pledge specifications.
--
-- Operators: '·' (concatenation), '/\' (conjunction), '\\' (subtraction),
-- with constants 'empty', 'universe'.
--
-- Laws for '·':
--
-- * Associativity:     @(a '·' b) '·' c = a '·' (b '·' c)@
-- * Left  identity:    @'empty' '·' a = a@
-- * Right identity:    @a '·' 'empty' = a@
--
-- Laws for '/\':
--
-- * Associativity:     @(a '/\' b) '/\' c = a '/\' (b '/\' c)@
-- * Commutativity:     @a '/\' b = b '/\' a@
-- * Identity:          @'universe' '/\' a = a@
--
-- Laws for '\\':
--
-- * Right zero:        @a '\\' 'empty' = a@
-- * Universe residual: @'universe' '\\' a = 'universe'@
-- * Sequential dist.:  @x '\\' (a '·' b) = (x '\\' b) '\\' a@
-- * Conj. dist.:       @(a '/\' b) '\\' c = (a '\\' c) '/\' (b '\\' c)@
--
-- These laws are required for 'Pledge' to be a lawful 'Monad'.
