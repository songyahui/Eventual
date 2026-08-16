module Pledge.Presburger
    ( -- * Re-exported from Pledge.Event
      module Pledge.Event
      -- * Presburger expressions
    , PExpr(..)
    , pNeg
      -- * Presburger predicates
    , PPred(..)
      -- * Normalization
    , normalizePExpr
    , normalizePPred
    ) where

import Data.List (nub)
import Pledge.Event

-- | A linear arithmetic expression over heap values.
-- Variables are heap addresses; @'ValAt' a@ dereferences address @a@.
-- Only scalar multiplication is allowed to preserve linearity.
data PExpr
    = Lit Int         -- ^ integer literal
    | ValAt Addr      -- ^ value at address: @h[a]@
    | Add PExpr PExpr -- ^ @e1 + e2@
    | Mul Int   PExpr -- ^ @k * e@  (scalar only, preserves linearity)
    deriving (Eq)

instance Show PExpr where
    show (Lit n)     = show n
    show (ValAt a)   = "h[" ++ show a ++ "]"
    show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Mul k e)   = show k ++ "*" ++ show e

-- | Negate a 'PExpr' by scalar multiplication with -1.
pNeg :: PExpr -> PExpr
pNeg = Mul (-1)

-- | A linear arithmetic predicate over heap values.
-- Used as the Presburger component in 'GuardedRE' and as pure assertions in 'SL'.
-- Satisfiability is discharged to Z3 via 'checkPPred'.
data PPred
    = PTrue
    | PFalse
    | PLt  PExpr PExpr  -- ^ @e1 < e2@
    | PLe  PExpr PExpr  -- ^ @e1 ≤ e2@
    | PEq  PExpr PExpr  -- ^ @e1 = e2@
    | PGt  PExpr PExpr  -- ^ @e1 > e2@
    | PGe  PExpr PExpr  -- ^ @e1 ≥ e2@
    | PNot PPred
    | PAnd PPred PPred
    deriving (Eq)

instance Show PPred where
    show PTrue        = "true"
    show PFalse       = "false"
    show (PLt  e1 e2) = show e1 ++ " < "  ++ show e2
    show (PLe  e1 e2) = show e1 ++ " ≤ "  ++ show e2
    show (PEq  e1 e2) = show e1 ++ " = "  ++ show e2
    show (PGt  e1 e2) = show e1 ++ " > "  ++ show e2
    show (PGe  e1 e2) = show e1 ++ " ≥ "  ++ show e2
    show (PNot p)     = "¬(" ++ show p ++ ")"
    show (PAnd p q)   = "(" ++ show p ++ " ∧ " ++ show q ++ ")"

-- ── Normalization ─────────────────────────────────────────────────────────────

-- | Simplify a 'PExpr' using arithmetic identities.
normalizePExpr :: PExpr -> PExpr
normalizePExpr (Add e1 e2) = case (normalizePExpr e1, normalizePExpr e2) of
    (Lit 0, e')    -> e'
    (e',    Lit 0) -> e'
    (Lit a, Lit b) -> Lit (a + b)
    (e1',   e2')   -> Add e1' e2'
normalizePExpr (Mul k e) = case normalizePExpr e of
    _      | k == 0 -> Lit 0
    e'     | k == 1 -> e'
    Lit n           -> Lit (k * n)
    e'              -> Mul k e'
normalizePExpr e = e   -- Lit, ValAt: already normal

-- | Simplify a 'PPred' by:
--   * eliminating @PTrue@ from @PAnd@ (identity)
--   * short-circuiting on @PFalse@ (absorbing element)
--   * deduplicating conjuncts (idempotency)
--   * substituting equalities of the form @h[a]=k@ into sibling conjuncts
--   * eliminating double negation
--   * evaluating comparisons between literals
normalizePPred :: PPred -> PPred
normalizePPred p =
    let conjuncts = flattenAnd (normStep p)
        deduped   = nub conjuncts
        eqs       = [ (a, k) | PEq (ValAt a) (Lit k) <- deduped ]
                 ++ [ (a, k) | PEq (Lit k) (ValAt a) <- deduped ]
        subbed    = map (normStep . substEqs eqs) deduped
    in if PFalse `elem` subbed
       then PFalse
       else rebuildAnd (nub (filter (/= PTrue) subbed))

-- One-level structural simplification (no flattening).
normStep :: PPred -> PPred
normStep (PAnd p q) = case (normStep p, normStep q) of
    (PFalse, _)          -> PFalse
    (_, PFalse)          -> PFalse
    (PTrue, q')          -> q'
    (p',    PTrue)       -> p'
    (p',    q') | p'==q' -> p'
    (p',    q')          -> PAnd p' q'
normStep (PNot p) = case normStep p of
    PTrue   -> PFalse
    PFalse  -> PTrue
    PNot p' -> p'
    p'      -> PNot p'
normStep (PLt  e1 e2) = evalCmp PLt  (<)  e1 e2
normStep (PLe  e1 e2) = evalCmp PLe  (<=) e1 e2
normStep (PEq  e1 e2) = evalCmp PEq  (==) e1 e2
normStep (PGt  e1 e2) = evalCmp PGt  (>)  e1 e2
normStep (PGe  e1 e2) = evalCmp PGe  (>=) e1 e2
normStep p            = p

-- Flatten a right/left-associated PAnd tree into a list of conjuncts.
flattenAnd :: PPred -> [PPred]
flattenAnd (PAnd p q) = flattenAnd p ++ flattenAnd q
flattenAnd PTrue      = []
flattenAnd p          = [p]

-- Rebuild a flat list of conjuncts back into a PAnd tree ([] → PTrue).
rebuildAnd :: [PPred] -> PPred
rebuildAnd []     = PTrue
rebuildAnd [p]    = p
rebuildAnd (p:ps) = PAnd p (rebuildAnd ps)

-- Substitute a list of (addr → literal) equalities into a PPred.
substEqs :: [(Addr, Int)] -> PPred -> PPred
substEqs eqs = goP
  where
    goE (ValAt a)   = maybe (ValAt a) Lit (lookup a eqs)
    goE (Add e1 e2) = normalizePExpr (Add (goE e1) (goE e2))
    goE (Mul k e)   = normalizePExpr (Mul k (goE e))
    goE e           = e

    goP (PLt  e1 e2) = normStep (PLt  (goE e1) (goE e2))
    goP (PLe  e1 e2) = normStep (PLe  (goE e1) (goE e2))
    goP (PEq  e1 e2) = normStep (PEq  (goE e1) (goE e2))
    goP (PGt  e1 e2) = normStep (PGt  (goE e1) (goE e2))
    goP (PGe  e1 e2) = normStep (PGe  (goE e1) (goE e2))
    goP (PNot q)     = normStep (PNot (goP q))
    goP (PAnd p q)   = normStep (PAnd (goP p) (goP q))
    goP p            = p

-- Normalize subexpressions and fold literal comparisons to PTrue/PFalse.
evalCmp :: (PExpr -> PExpr -> PPred) -> (Int -> Int -> Bool) -> PExpr -> PExpr -> PPred
evalCmp con op e1 e2 =
    case (normalizePExpr e1, normalizePExpr e2) of
        (Lit a, Lit b) -> if op a b then PTrue else PFalse
        (e1',  e2')    -> con e1' e2'
