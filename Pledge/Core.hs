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
-- A single m-action that produces the return value together with its pre-,
-- post-, and future-conditions all at once.  Because all four components come
-- from one run of the m action, stateful resources (e.g. file handles) are
-- used exactly once per invocation.

newtype Pledge m eff a = Pledge { runPledge :: m (a, eff, eff, eff) }
--                                                ^   ^    ^    ^
--                                               ret pre  post future

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

-- Monad laws hold when 'Composable' satisfies:
--   empty · a = a,  a · empty = a,  (a · b) · c = a · (b · c)
--   universe /\ a = a
--   a \\ empty = a,  universe \\ a = universe
--   x \\ (a · b) = (x \\ b) \\ a
--   (a /\ b) \\ c = (a \\ c) /\ (b \\ c)
