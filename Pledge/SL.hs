module Pledge.SL
    ( -- * Heap type
      Heap
      -- * Separation logic predicates
    , SL(..)
    , normalizeSL
    ) where

import Prelude hiding ((<>))
import qualified Data.Map.Strict as Map
import Pledge.Core
import Pledge.Presburger

-- | A concrete heap: a finite map from addresses to integer values.
type Heap = Map.Map Addr Val

-- | Symbolic separation-logic predicates over integer-addressed heaps.
--
-- 'SL' is the 'Composable' instance for heap-ownership reasoning.
-- The algebra mirrors the RE algebra:
--
-- * 'SepStar' / 'Emp'  ↔  'Seq' / 'Epsilon'  (separating conjunction / identity)
-- * 'Conj' / 'Top'     ↔  'And' / Σ*          (ordinary conjunction / identity)
-- * 'Wand'             ↔  Brzozowski quotient  (magic-wand residual)
data SL
    = Emp              -- ^ empty heap (identity for 'SepStar')
    | Top              -- ^ any heap, unconstrained (identity for 'Conj')
    | Pure PPred       -- ^ a pure arithmetic predicate over heap values
    | Cell Addr Val    -- ^ singleton cell: address @a@ holds value @v@
    | SepStar SL SL    -- ^ separating conjunction @P * Q@
    | Conj   SL SL     -- ^ ordinary conjunction @P ∧ Q@
    | Wand   SL SL     -- ^ magic wand @P -* Q@: if given @P@, produces @Q@
    deriving (Eq, Show)

-- | Simplify an 'SL' predicate using structural identities:
-- @'Emp' * Q = Q@, @⊤ ∧ Q = Q@, @P ∧ P = P@, @'Emp' -* Q = Q@, @P -* ⊤ = ⊤@.
normalizeSL :: SL -> SL
normalizeSL s = case s of

    SepStar p q -> case (normalizeSL p, normalizeSL q) of
        (Emp, q')             -> q'           -- Emp * Q = Q
        (p',  Emp)            -> p'           -- P * Emp = P
        (p',  q')             -> SepStar p' q'

    Conj p q -> case (normalizeSL p, normalizeSL q) of
        (Top, q')             -> q'           -- ⊤ ∧ Q = Q
        (p',  Top)            -> p'           -- P ∧ ⊤ = P
        (p',  q') | p' == q' -> p'           -- P ∧ P = P  (idempotent)
        (p',  q')             -> Conj p' q'

    Wand p q -> case (normalizeSL p, normalizeSL q) of
        (Emp, q')             -> q'           -- Emp -* Q = Q
        (_,   Top)            -> Top          -- P -* ⊤ = ⊤
        (p',  q')             -> Wand p' q'

    _ -> s

-- ── Composable instance ───────────────────────────────────────────────────────

instance Composable SL where
    concatenation = SepStar
    conjunction   = Conj
    empty         = Emp
    universe      = Top

    -- emp -* Q = Q: providing nothing leaves the obligation unchanged.
    subtraction Emp q = q
    -- Contract convention: ⊤ precondition is trivially discharged.
    subtraction Top _ = Emp
    -- General case: symbolic magic wand.
    subtraction p   q = Wand p q
