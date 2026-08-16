module Pledge.Event
    ( -- * Terms
      Term(..)
      -- * Events
    , Event(..)
    , subsumesEvent
      -- * Shared type aliases
    , Addr
    , Val
    ) where

import Data.List (intercalate)

-- ── Terms ─────────────────────────────────────────────────────────────────────
-- A structured value that can be carried by an event.

data Term = Str String | Num Int | List [Term]
    deriving (Eq)

instance Show Term where
    show (Str s)   = "\"" ++ s ++ "\""
    show (Num n)   = show n
    show (List ts) = "[" ++ intercalate ", " (map show ts) ++ "]"

-- ── Events ────────────────────────────────────────────────────────────────────
-- An Event is either a concrete named call or a wildcard pattern (Σ).
-- Wildcard is used only inside RE patterns (Single Wildcard ≡ Σ, any one step).

data Event t = Atom String t   -- e.g. send(x)
             | Wildcard         -- matches any single event
    deriving (Eq)

instance Show t => Show (Event t) where
    show (Atom name arg) = name ++ "(" ++ show arg ++ ")"
    show Wildcard        = "_"

-- | Does a concrete event occurrence match an event pattern?
-- A 'Wildcard' pattern matches any occurrence; a 'Wildcard' occurrence does
-- not match a specific 'Atom' pattern.
subsumesEvent :: Eq t => Event t -> Event t -> Bool
subsumesEvent _            Wildcard      = True
subsumesEvent (Atom n1 a1) (Atom n2 a2) = n1 == n2 && a1 == a2
subsumesEvent Wildcard     (Atom _ _)   = False

-- ── Shared type aliases ───────────────────────────────────────────────────────
-- Used by Presburger expressions, GuardedRE, and the SL instance.

type Addr = Int
type Val  = Int
