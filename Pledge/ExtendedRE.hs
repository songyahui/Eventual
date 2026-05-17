{-# OPTIONS_GHC -i.. #-}
module Pledge.ExtendedRE
    ( -- * Type
      ExtendedRE(..)
      -- * Construction
    , fromRE
    , fromPPred
      -- * Conjunction
    , conjoin
      -- * Derivatives
    , deriveExt
      -- * Membership
    , nullableExt
    , checkExt
    ) where

import qualified Data.Map.Strict as Map
import Pledge.Core
import Pledge.Presburger
import Pledge.RE

-- ── Type ──────────────────────────────────────────────────────────────────────
-- An ExtendedRE is a conjunction of two independent constraints on a state:
--   • a Presburger predicate over the heap
--   • a regular expression over the event trace
--
-- A state (heap, trace) satisfies ExtendedRE p r  iff
--     heap  |= p           (Presburger side)
--   ∧ trace ∈ L(r)         (trace side)

data ExtendedRE = ExtendedRE PPred RE
    deriving (Eq)

instance Show ExtendedRE where
    show (ExtendedRE PTrue r) = show r
    show (ExtendedRE p     r) = "[" ++ show p ++ "] ∧ " ++ show r

-- ── Construction ──────────────────────────────────────────────────────────────

-- Lift a plain RE (no heap constraint).
fromRE :: RE -> ExtendedRE
fromRE = ExtendedRE PTrue

-- Lift a plain PPred (no trace constraint: accept any trace).
fromPPred :: PPred -> ExtendedRE
fromPPred p = ExtendedRE p top

-- ── Conjunction ───────────────────────────────────────────────────────────────
-- (p1, r1) ∧ (p2, r2)  =  (p1 ∧ p2, r1 ∩ r2)
-- Both the heap and the trace must satisfy both constraints.

conjoin :: ExtendedRE -> ExtendedRE -> ExtendedRE
conjoin (ExtendedRE p1 r1) (ExtendedRE p2 r2) =
    ExtendedRE (normalizePPred (PAnd p1 p2)) (normalize (And r1 r2))

-- ── Derivatives ───────────────────────────────────────────────────────────────
-- Consuming an event advances only the trace side; the heap predicate is
-- a static constraint and does not change with individual events.

deriveExt :: Event -> ExtendedRE -> ExtendedRE
deriveExt e (ExtendedRE p r) = ExtendedRE p (normalize (derivative e r))

-- ── Membership ────────────────────────────────────────────────────────────────

-- nullableExt checks whether (heap, ε) satisfies the ExtendedRE:
--   • the RE must be nullable (ε ∈ L(r))
--   • the Presburger predicate must be satisfiable against the heap
--
-- The heap is a concrete assignment Map Addr Int; we instantiate the
-- predicate with those values and ask the solver.
nullableExt :: Map.Map Addr Int -> ExtendedRE -> IO Bool
nullableExt heap (ExtendedRE p r)
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

-- checkExt heap trace ext: does (heap, trace) satisfy ext?
-- Folds deriveExt over the trace then checks nullability.
checkExt :: Map.Map Addr Int -> [Event] -> ExtendedRE -> IO Bool
checkExt heap trace ext =
    nullableExt heap (foldl (flip deriveExt) ext trace)

-- ── Composable instance ───────────────────────────────────────────────────────
-- Lifts the RE algebra to ExtendedRE, threading PPred as a conjunction.
--
-- subtraction: both heap predicates are conjoined in the residual, so the
-- full constraint from both sides is preserved; the RE side uses reSubtraction.

instance Composable ExtendedRE where
    concatenation (ExtendedRE p1 r1) (ExtendedRE p2 r2) =
        ExtendedRE (normalizePPred (PAnd p1 p2)) (normalize (Seq r1 r2))
    conjunction   = conjoin
    empty         = ExtendedRE PTrue Epsilon
    universe      = ExtendedRE PTrue top
    subtraction   (ExtendedRE p1 r1) (ExtendedRE p2 r2) =
        ExtendedRE (normalizePPred (PAnd p1 p2)) (normalize (reSubtraction r1 r2))
