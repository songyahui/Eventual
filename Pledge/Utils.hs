{-# OPTIONS_GHC -i.. #-}
module Pledge.Utils
    ( -- * Terms
      Term(..)
      -- * Events
    , Event(..)
    , subsumesEvent
      -- * Shared type aliases
    , Addr
    , Val
      -- * Presburger expressions
    , PExpr(..)
    , pNeg
      -- * Presburger predicates
    , PPred(..)
    ) where

import Data.List (intercalate)

-- ── Terms ─────────────────────────────────────────────────────────────────────

data Term = Var String | Str String | Num Int | List [Term]
    deriving (Eq)

instance Show Term where
    show (Var s) = s
    show (Str s) = "\"" ++ s ++ "\""
    show (Num n) = show n
    show (List ts) = "[" ++ intercalate ", " (map show ts) ++ "]"

-- ── Events ────────────────────────────────────────────────────────────────────

-- An Event is either a concrete named call or a wildcard pattern (Σ).
-- Wildcard is used only inside RE patterns (Single Wildcard ≡ Σ, any one step).
data Event = Atom String Term    -- e.g. send(x)
           | Wildcard            -- matches any single event
    deriving (Eq)

instance Show Event where
    show (Atom name arg) = name ++ "(" ++ show arg ++ ")"
    show Wildcard         = "_"

-- Does a concrete event occurrence match an event pattern in a Single?
subsumesEvent :: Event -> Event -> Bool
subsumesEvent _            Wildcard      = True   -- any occurrence matches wildcard pattern
subsumesEvent (Atom n1 a1) (Atom n2 a2) = n1 == n2 && a1 == a2
subsumesEvent Wildcard     (Atom _ _)   = False   -- wildcard occurrence ≠ specific pattern

-- ── Shared type aliases ───────────────────────────────────────────────────────
-- Addr and Val are used by both RE examples and the SL instance.

type Addr = Int
type Val  = Int

-- ── Presburger Arithmetic ─────────────────────────────────────────────────────
-- Linear arithmetic over heap values.  Variables are heap addresses; ValAt a
-- dereferences address a.  Only scalar multiplication (k * e) is allowed to
-- keep expressions linear.

data PExpr
    = Lit Int           -- integer literal
    | ValAt Addr        -- value stored at address: h[a]
    | Add PExpr PExpr   -- e1 + e2
    | Mul Int   PExpr   -- k * e  (scalar only, preserves linearity)
    deriving (Eq)

instance Show PExpr where
    show (Lit n)     = show n
    show (ValAt a)   = "h[" ++ show a ++ "]"
    show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Mul k e)   = show k ++ "*" ++ show e

-- Derived expression smart constructors
pNeg :: PExpr -> PExpr
pNeg = Mul (-1)

data PPred
    = PTrue
    | PLt  PExpr PExpr  -- e1 < e2
    | PLe  PExpr PExpr  -- e1 ≤ e2
    | PEq  PExpr PExpr  -- e1 = e2
    | PGt  PExpr PExpr  -- e1 > e2
    | PGe  PExpr PExpr  -- e1 ≥ e2
    | PNot PPred
    | PAnd PPred PPred
    deriving (Eq)

instance Show PPred where
    show PTrue        = "true"
    show (PLt  e1 e2) = show e1 ++ " < "  ++ show e2
    show (PLe  e1 e2) = show e1 ++ " ≤ "  ++ show e2
    show (PEq  e1 e2) = show e1 ++ " = "  ++ show e2
    show (PGt  e1 e2) = show e1 ++ " > "  ++ show e2
    show (PGe  e1 e2) = show e1 ++ " ≥ "  ++ show e2
    show (PNot p)     = "¬(" ++ show p ++ ")"
    show (PAnd p q)   = "(" ++ show p ++ " ∧ " ++ show q ++ ")"

-- ── Composable class ──────────────────────────────────────────────────────────
