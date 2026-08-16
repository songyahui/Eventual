module Pledge.Semiring
    ( -- * Class
      Semiring(..)
      -- * Instances
    , Prob(..)
    , Tropical(..)
    ) where

-- | A semiring @(w, ⊕, ⊗, 0, 1)@ generalising the Boolean lattice.
--
-- @
-- ⊕  = choice     (||  / union  / min  / capped-+)
-- ⊗  = sequence   (&&  / concat / +    / ×)
-- 0  = zero       (⊥   / ∅     / ∞    / 0)
-- 1  = unit       (⊤   / Σ*    / 0    / 1)
-- @
--
-- Laws: @⊗@ distributes over @⊕@; @0@ is absorbing for @⊗@; @1@ is the
-- identity for @⊗@.  The 'Bool' instance recovers plain 'RE' membership.
class (Eq w, Show w) => Semiring w where
    -- | Additive identity; also absorbing element for 'smul'.
    szero :: w
    -- | Multiplicative identity.
    sone  :: w
    -- | Additive operation (choice / union).
    sadd  :: w -> w -> w
    -- | Multiplicative operation (sequence / composition).
    smul  :: w -> w -> w

-- Boolean semiring — recovers the existing RE / Composable behaviour exactly.
instance Semiring Bool where
    szero = False
    sone  = True
    sadd  = (||)
    smul  = (&&)

-- | Probability semiring: weights represent the probability that an obligation
-- will be met.  'smul' models independent sequential obligations (@p * q@).
--
-- __Warning__: 'sadd' uses saturating addition (clamped to 1.0), making this
-- a /saturating/ semiring rather than a true algebraic one — the distributivity
-- law @a ⊗ (b ⊕ c) = (a ⊗ b) ⊕ (a ⊗ c)@ can fail when @b + c > 1@.
-- Use 'Prob' only where approximate upper-bound reasoning is acceptable.
newtype Prob = Prob { getProb :: Double }
    deriving (Eq, Ord)

instance Show Prob where
    show (Prob p) = show (round (p * 100) :: Int) ++ "%"

instance Semiring Prob where
    szero                      = Prob 0
    sone                       = Prob 1
    sadd (Prob a) (Prob b)     = Prob (min 1.0 (a + b))
    smul (Prob a) (Prob b)     = Prob (a * b)

-- | Tropical (min-plus) semiring: weights represent the minimum cost (e.g.
-- expected number of steps) to discharge an obligation.
--
-- * 'sadd' = @min@ — choose the cheaper path.
-- * 'smul' = @(+)@ — costs accumulate along a path.
-- * 'szero' = @∞@  — unreachable / no valid path exists.
-- * 'sone'  = @0@  — zero cost / obligation already discharged.
newtype Tropical = Tropical { getCost :: Double }
    deriving (Eq, Ord)

instance Show Tropical where
    show (Tropical c)
        | isInfinite c = "∞ steps"
        | otherwise    = show (round c :: Int) ++ " steps"

instance Semiring Tropical where
    szero                          = Tropical (1/0)
    sone                           = Tropical 0
    sadd (Tropical a) (Tropical b) = Tropical (min a b)
    smul (Tropical a) (Tropical b) = Tropical (a + b)
