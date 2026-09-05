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

-- | A structured payload that can be carried by a concrete 'Event'.
-- Terms are used as the default payload type throughout the library.
data Term
    = Str  String   -- ^ a string literal, e.g. a file path
    | Num  Int      -- ^ an integer, e.g. a heap address or counter value
    | List [Term]   -- ^ a heterogeneous list of terms
    deriving (Eq)

instance Show Term where
    show (Str s)   = "\"" ++ s ++ "\""
    show (Num n)   = show n
    show (List ts) = "[" ++ intercalate ", " (map show ts) ++ "]"

-- | A single observable event, parameterised by the payload type @t@.
--
-- Events are the alphabet of the temporal specification languages ('RE', 'WRE', …).
-- Concrete occurrences use 'Atom'; pattern positions inside 'RE' use 'Wildcard'
-- and 'Usage'.
data Event t
    = Atom String t  -- ^ a named event with a typed payload, e.g. @send(42)@
    | Usage t        -- ^ pattern that matches any /named/ event carrying this
                      -- exact payload, whatever its name — e.g.\ @Usage addr@
                      -- matches both @malloc(addr)@ and @free(addr)@
    | Wildcard       -- ^ pattern that matches /any/ single event (used in RE only)
    deriving (Eq)

instance Show t => Show (Event t) where
    show (Atom name arg) = name ++ "(" ++ show arg ++ ")"
    show (Usage arg)     = "_(" ++ show arg ++ ")"
    show Wildcard        = "_"

-- | Does a concrete event occurrence match an event pattern?
-- A 'Wildcard' pattern matches any occurrence; a 'Wildcard' occurrence does
-- not match a specific 'Atom' or 'Usage' pattern.
-- A 'Usage' pattern matches any 'Atom' occurrence carrying the same payload,
-- regardless of name — it is name-agnostic where 'Atom' is name-exact.
subsumesEvent :: Eq t => Event t -> Event t -> Bool
subsumesEvent _            Wildcard      = True
subsumesEvent (Atom n1 a1) (Atom n2 a2) = n1 == n2 && a1 == a2
subsumesEvent (Atom _  a1) (Usage  a2)  = a1 == a2
subsumesEvent (Usage  a1)  (Atom _  a2) = a1 == a2
subsumesEvent (Usage  a1)  (Usage  a2)  = a1 == a2
subsumesEvent Wildcard     (Atom _ _)   = False
subsumesEvent Wildcard     (Usage _)    = False

-- ── Shared type aliases ───────────────────────────────────────────────────────

-- | A heap address (integer index into the symbolic heap).
type Addr = Int

-- | A heap value (integer stored at an 'Addr').
type Val  = Int
