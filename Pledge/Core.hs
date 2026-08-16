{-# OPTIONS_GHC -i.. #-}
{-# LANGUAGE FlexibleInstances #-}
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

-- future is now indexed by the return value (direction 1: data-dependent
-- future conditions).  This lets an operation's temporal obligation refer
-- to whatever resource handle it returns, e.g.
--   mallocFresh addr = Effectful { ..., future = \a -> finally(free(a)) }
-- so that the exact address returned drives the obligation.

data Pledge m eff a = Pledge
    { ret    :: m a
    , pre    :: m eff
    , post   :: a -> m eff
    , future :: a -> m eff
    }

instance (Monad m) => Functor (Pledge m eff) where
    -- fmap changes the return type from a to b, so post and future must become
    -- b -> m eff.  We ignore the new b argument and evaluate at the known
    -- original return value (the obligation is already determined).
    fmap f e = Pledge
        { ret    = fmap f (ret e)
        , pre    = pre e
        , post   = \_ -> ret e >>= post e
        , future = \_ -> ret e >>= future e
        }

instance (Composable eff, Monad m) => Applicative (Pledge m eff) where
    pure x = Pledge
        { ret    = pure x
        , pre    = pure universe
        , post   = const $ pure empty
        , future = const $ pure universe
        }
    f <*> x = Pledge
        { ret    = ret f <*> ret x
        -- pre(f <*> x) = preF /\ (preX \\ postF@retF)
        , pre    = pre f /\ (pre x \\ (ret f >>= post f))
        -- post(f <*> x) = postF@retF · postX@retX  (ignore the b result)
        , post   = \_ -> (ret f >>= post f) · (ret x >>= post x)
        -- future(f <*> x) = (futureF@retF \\ postX@retX) /\ futureX@retX
        , future = \_ -> ((ret f >>= future f) \\ (ret x >>= post x)) /\ (ret x >>= future x)
        }

instance (Composable eff, Monad m) => Monad (Pledge m eff) where
    return = pure
    e >>= f = Pledge
        { ret    = ret e >>= \a -> ret (f a)
        -- {P} e {Q},  {P'} (f a) {R}  ⊢  {P /\ (P' \\ Q)} e >>= f {R}
        , pre    = ret e >>= \a -> pre e /\ (pre (f a) \\ post e a)
        -- post and future ignore the final b; obligations are fixed by the bound a
        , post   = \_ -> ret e >>= \a -> post e a · (ret (f a) >>= post (f a))
        -- future(e >>= f) = (futureE@a \\ postFE@retFA) /\ futureFE@retFA
        , future = \_ -> ret e >>= \a -> (future e a \\ (ret (f a) >>= post (f a))) /\ (ret (f a) >>= future (f a))
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
