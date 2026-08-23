module Pledge.GuardedRE
    ( -- * Type
      GuardedRE(..)
      -- * Construction
    , fromRE
    , fromPPred
      -- * Conjunction
    , conjoin
      -- * Derivatives
    , deriveGuarded
      -- * Membership
    , nullableGuarded
    , checkGuarded
    ) where

import qualified Data.Map.Strict as Map
import Pledge.Core
import Pledge.Presburger
import Pledge.Presburger.Solver
import Pledge.RE

-- | A conjunction of a Presburger predicate and a regular expression,
-- enforcing heap invariants and trace-ordering obligations simultaneously.
--
-- A state @(heap, trace)@ satisfies @GuardedRE p r@ iff:
--
-- * @heap  |= p@   (Presburger predicate holds on the heap), and
-- * @trace ∈ L(r)@ (trace is in the language of the RE).
--
-- The two dimensions are independent: the Presburger predicate is static
-- (not advanced by events), while the RE is advanced by 'deriveGuarded'.
data GuardedRE a = GuardedRE PPred (RE a)
    deriving (Eq)

instance Show a => Show (GuardedRE a) where
    show (GuardedRE PTrue r) = show r
    show (GuardedRE p     r) = "[" ++ show p ++ "] ∧ " ++ show r

-- ── Construction ──────────────────────────────────────────────────────────────

-- | Lift a plain 'RE' into a 'GuardedRE' with no heap constraint (@PPred = PTrue@).
fromRE :: RE a -> GuardedRE a
fromRE = GuardedRE PTrue

-- | Lift a plain 'PPred' into a 'GuardedRE' with no trace constraint
-- (any trace is accepted: @RE = Σ*@).
fromPPred :: PPred -> GuardedRE a
fromPPred p = GuardedRE p top

-- ── Conjunction ───────────────────────────────────────────────────────────────

-- | Conjoin two 'GuardedRE' values component-wise:
-- @(p1, r1) ∧ (p2, r2) = (p1 ∧ p2, r1 ∩ r2)@.
-- Both the heap predicate and the trace RE must hold simultaneously.
conjoin :: Eq a => GuardedRE a -> GuardedRE a -> GuardedRE a
conjoin (GuardedRE p1 r1) (GuardedRE p2 r2) =
    GuardedRE (normalizePPred (PAnd p1 p2)) (normalize (And r1 r2))

-- ── Derivatives ───────────────────────────────────────────────────────────────

-- | Advance the trace side of a 'GuardedRE' by one event.
-- The 'PPred' component is static and is left unchanged.
deriveGuarded :: Eq a => Event a -> GuardedRE a -> GuardedRE a
deriveGuarded e (GuardedRE p r) = GuardedRE p (normalize (derivative e r))

-- ── Membership ────────────────────────────────────────────────────────────────

-- | Check whether @(heap, ε)@ satisfies a 'GuardedRE':
-- the RE must be nullable and the 'PPred' must be satisfiable
-- under the concrete @heap@ assignment.  Uses Z3 via SBV.
nullableGuarded :: Map.Map Addr Int -> GuardedRE a -> IO Bool
nullableGuarded heap (GuardedRE p r)
    | not (nullable r) = return False
    | otherwise        = do
        result <- checkPPred (instantiate heap p)
        return $ case result of
            Satisfied _ -> True
            _           -> False

-- Instantiate a PPred by substituting concrete heap values for ValAt.
-- Any address not present in the map is left as a free variable.
instantiate :: Map.Map Addr Int -> PPred -> PPred
instantiate heap = go
  where
    subst (Lit n)     = Lit n
    subst (ValAt a)   = maybe (ValAt a) Lit (Map.lookup a heap)
    subst (Add e1 e2) = Add (subst e1) (subst e2)
    subst (Mul k e)   = Mul k (subst e)

    go PTrue        = PTrue
    go (PLt  e1 e2) = PLt  (subst e1) (subst e2)
    go (PLe  e1 e2) = PLe  (subst e1) (subst e2)
    go (PEq  e1 e2) = PEq  (subst e1) (subst e2)
    go (PGt  e1 e2) = PGt  (subst e1) (subst e2)
    go (PGe  e1 e2) = PGe  (subst e1) (subst e2)
    go (PNot q)     = PNot (go q)
    go (PAnd q1 q2) = PAnd (go q1) (go q2)

-- | Check whether @(heap, trace)@ satisfies a 'GuardedRE'.
-- Folds 'deriveGuarded' over the trace and then calls 'nullableGuarded'.
checkGuarded :: Eq a => Map.Map Addr Int -> [Event a] -> GuardedRE a -> IO Bool
checkGuarded heap trace ext =
    nullableGuarded heap (foldl (flip deriveGuarded) ext trace)

-- ── Composable instance ───────────────────────────────────────────────────────
-- Lifts the RE algebra to GuardedRE, threading PPred as a conjunction.
--
-- leftQuotient/rightQuotient: both heap predicates are conjoined in the
-- residual; the RE side uses reLeftQuotient / reRightQuotient.

instance Eq a => Composable (GuardedRE a) where
    concatenation (GuardedRE p1 r1) (GuardedRE p2 r2) =
        GuardedRE (normalizePPred (PAnd p1 p2)) (normalize (Seq r1 r2))
    conjunction   = conjoin
    empty         = GuardedRE PTrue Epsilon
    universe      = GuardedRE PTrue top
    leftQuotient  (GuardedRE p1 r1) (GuardedRE p2 r2) =
        GuardedRE (normalizePPred (PAnd p1 p2)) (normalize (reLeftQuotient r1 r2))
    rightQuotient (GuardedRE p1 r1) (GuardedRE p2 r2) =
        GuardedRE (normalizePPred (PAnd p1 p2)) (normalize (reRightQuotient r1 r2))
