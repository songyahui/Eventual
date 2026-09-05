module Pledge.Core
    ( -- * Composable class
      Composable(..)
    , (·)
    , (⊓)
    , (∖)
    , (∕)
    , ε
    , (⊤)
      -- * Pledge monad
    , Pledge(..)
    , liftPledge
      -- * Safe inspection (run once)
    , PledgeResult(..)
    , inspect
    , assertState
      -- * Component accessors (pure \/ effect-free @m@ only)
    , getRet
    , getPre
    , getPost
    , getFut
    ) where

-- | Abstract algebra shared by all temporal-specification types.
--
-- An instance must satisfy the laws in the 'Pledge' monad source comments:
--
-- * @'concatenation'@ is associative with unit @'empty'@.
-- * @'conjunction'@ is associative and commutative with unit @'universe'@.
-- * @'leftQuotient' q p@ is the left-quotient of @p@ by @q@.
-- * @'rightQuotient' q p@ is the right-quotient of @p@ by @q@.
class Composable a where
    -- | Sequential composition: RE concatenation, @*@ in SL, @⊗@ in WRE.
    concatenation :: a -> a -> a
    -- | Simultaneous constraint: RE intersection, @⊓@ in SL, pointwise @⊗@ in WRE.
    conjunction   :: a -> a -> a
    -- | Identity for 'concatenation': @ε@ in RE, @emp@ in SL.
    empty         :: a
    -- | Identity for 'conjunction': @Σ*@ in RE, @⊤@ in SL.
    universe      :: a
    -- | Left-quotient: trace subtraction.  Infix alias: @post '∖' pre@ (see '(∖)').
    leftQuotient   :: a -> a -> a
    -- | Right-quotient: pre-residual.  Infix alias: @pre '∕' post@ (see '(∕)').
    rightQuotient   :: a -> a -> a

-- | Component-wise 'Composable' instance for pairs.
instance (Composable a, Composable b) => Composable (a, b) where
    concatenation (a1, b1) (a2, b2) = (concatenation a1 a2, concatenation b1 b2)
    conjunction   (a1, b1) (a2, b2) = (conjunction   a1 a2, conjunction   b1 b2)
    leftQuotient  (a1, b1) (a2, b2) = (leftQuotient  a1 a2, leftQuotient  b1 b2)
    rightQuotient (a1, b1) (a2, b2) = (rightQuotient a1 a2, rightQuotient b1 b2)
    empty                           = (empty, empty)
    universe                        = (universe, universe)

-- | Lifts 'Composable' pointwise through any 'Applicative'.
instance {-# OVERLAPPABLE #-} (Composable eff, Applicative m) => Composable (m eff) where
    concatenation = liftA2 concatenation
    conjunction   = liftA2 conjunction
    leftQuotient  = liftA2 leftQuotient
    rightQuotient = liftA2 rightQuotient
    empty         = pure empty
    universe      = pure universe

infixl 6 ·
-- | Infix alias for 'concatenation' (@infixl 6@).
(·) :: Composable a => a -> a -> a
(·) = concatenation

infixl 7 ⊓
-- | Infix alias for 'conjunction' (@infixl 7@).
(⊓) :: Composable a => a -> a -> a
(⊓) = conjunction

infixl 5 ∖
-- | Left-quotient / post-residual (@infixl 5@): @future ∖ post@
-- is the residual of @future@ after consuming @post@.
-- Defined as @fut '∖' post = leftQuotient post fut@.
(∖) :: Composable a => a -> a -> a
a ∖ b = leftQuotient b a

infixl 5 ∕
-- | Right-quotient / pre-residual (@infixl 5@): @pre '∕' post@
-- is the residual of @pre@ after consuming @post@.
-- Defined as @pre '∕' post = rightQuotient post pre@.
(∕) :: Composable a => a -> a -> a
p ∕ q = rightQuotient q p

-- | Alias for 'empty': identity for '(·)' (@ε@ in RE, @emp@ in SL).
ε :: Composable a => a
ε = empty

-- | Alias for 'universe': identity for '(⊓)' (@Σ*@ in RE, @⊤@ in SL).
(⊤) :: Composable a => a
(⊤) = universe

-- ── Pledge monad ─────────────────────────────────────────────────────────────

-- | A monadic action in @m@ carrying temporal specifications in @eff@.
--
-- Each action produces @(ret, pre, post, fut) :: (a, eff, eff, eff)@:
--
-- * @ret@  — return value.
-- * @pre@  — precondition: what the preceding trace must satisfy.
-- * @post@ — postcondition: what this action emits.
-- * @fut@  — future obligation: what subsequent actions must discharge.
--
-- All four components come from a single run of the @m@ action, so
-- resources are allocated once and @ret@ is in scope when building @fut@.

newtype Pledge m eff a = Pledge { runPledge :: m (a, eff, eff, eff) }

-- | Lift a plain @m@ action into 'Pledge' with trivial conditions:
-- @pre = universe@, @post = empty@, @fut = universe@.
liftPledge :: (Composable eff, Applicative m) => m a -> Pledge m eff a
liftPledge ma = Pledge $ fmap (, universe, empty, universe) ma

-- | All four components of a completed 'Pledge' action, collected in one run.
data PledgeResult eff a = PledgeResult
    { ret  :: a    -- ^ the return value
    , pre  :: eff  -- ^ precondition (what must have held before)
    , post :: eff  -- ^ postcondition (what this action emitted)
    , fut  :: eff  -- ^ future condition (what must still hold after)
    }

-- | Run a 'Pledge' action once and collect all four components.
-- Prefer over the individual accessors when @m@ has side effects,
-- since each accessor calls 'runPledge' separately.
inspect :: Functor m => Pledge m eff a -> m (PledgeResult eff a)
inspect (Pledge ma) =
    fmap (\(a, pre, post, fut) -> PledgeResult a pre post fut) ma

assertState :: (Composable eff, Applicative m) => eff -> eff -> Pledge m eff ()
assertState post fut = Pledge $ pure ((), universe, post, fut)

-- | Extract the return value.
-- /Warning/: calls 'runPledge' independently — use 'inspect' when @m@ is 'IO'.
getRet :: Functor m => Pledge m eff a -> m a
getRet = fmap (\(ret, _, _, _) -> ret) . runPledge

-- | Extract the precondition.
-- /Warning/: calls 'runPledge' independently — use 'inspect' when @m@ is 'IO'.
getPre :: Functor m => Pledge m eff a -> m eff
getPre = fmap (\(_, pre, _, _) -> pre) . runPledge

-- | Extract the postcondition.
-- /Warning/: calls 'runPledge' independently — use 'inspect' when @m@ is 'IO'.
getPost :: Functor m => Pledge m eff a -> m eff
getPost = fmap (\(_, _, post, _) -> post) . runPledge

-- | Extract the future condition.
-- /Warning/: calls 'runPledge' independently — use 'inspect' when @m@ is 'IO'.
getFut :: Functor m => Pledge m eff a -> m eff
getFut = fmap (\(_, _, _, fut) -> fut) . runPledge

instance Functor m => Functor (Pledge m eff) where
    fmap f (Pledge ma) =
        Pledge $ fmap (\(a, pre, post, fut) -> (f a, pre, post, fut)) ma

instance (Composable eff, Monad m) => Applicative (Pledge m eff) where
    pure x = Pledge $ pure (x, universe, empty, universe)
    Pledge mf <*> Pledge mx = Pledge $ do
        (f, preF, postF, futF) <- mf
        (x, preX, postX, futX) <- mx
        return (f x, preF ⊓ (preX ∕ postF), postF · postX, (futF ∖ postX) ⊓ futX)

instance (Composable eff, Monad m) => Monad (Pledge m eff) where
    return = pure
    Pledge ma >>= g = Pledge $ do
        (a, preA, postA, futA) <- ma
        (b, preB, postB, futB) <- runPledge (g a)
        return (b, preA ⊓ (preB ∕ postA), postA · postB, (futA ∖ postB) ⊓ futB)

-- ── Monad law proofs ──────────────────────────────────────────────────────────
--
-- Notation: write a Pledge as (ret, pre, post, fut).
--
--   pure x          = (x, universe, empty, universe)
--   (P,Q,F) >>= g   -- where g _ = (ret', P', Q', F')
--     = (ret', P ⊓ (P' ∕ Q),  Q · Q',  (F ∖ Q') ⊓ F')
--
-- Required laws for (·), (⊓), (∖), and (∕):
--
--   (S1)  empty · a            = a                    left  identity of (·)
--   (S2)  a · empty            = a                    right identity of (·)
--   (S3)  (a · b) · c          = a · (b · c)          associativity  of (·)
--   (C1)  a ⊓ b                = b ⊓ a                commutativity of (⊓)
--   (C2)  (a ⊓ b) ⊓ c          = a ⊓ (b ⊓ c)          associativity of (⊓)
--   (C3)  universe ⊓ a         = a                    left  identity of (⊓)
--   (L1)  a ∖ empty            = a                    empty post discharges nothing
--   (L2)  universe ∖ a         = universe             universe is stable under ∖
--   (L3)  x ∖ (a · b)          = (x ∖ a) ∖ b          left-quotient sequential law
--   (L4)  (a ⊓ b) ∖ c          = (a ∖ c) ⊓ (b ∖ c)    ∖ distributes over (⊓)
--   (R1)  x ∕ empty            = x                    mirrors L1
--   (R2)  universe ∕ a         = universe             mirrors L2
--   (R3)  (x ∕ b) ∕ a          = x ∕ (a · b)          right-quotient sequential law
--   (R4)  (a ⊓ b) ∕ c          = (a ∕ c) ⊓ (b ∕ c)    mirrors L4
--
-- ── Law 1: left identity — pure a >>= f = f a ─────────────────────────────────
--
-- pure a = (a, universe, empty, universe).
-- Let f a = (b, P', Q', F').  Then pure a >>= f gives:
--
--   pre  : universe ⊓ (P' ∕ empty)
--        = universe ⊓ P'    -- by R1
--        = P'               -- by C3
--
--   post : empty · Q' = Q'                   -- by S1
--
--   fut  : (universe ∖ Q') ⊓ F'
--        = universe ⊓ F'                     -- by L2
--        = F'                                -- by C3
--
-- All three components equal those of f a.                                    □
--
-- ── Law 2: right identity — m >>= pure = m ────────────────────────────────────
--
-- Let m = (a, P, Q, F).  pure a = (a, universe, empty, universe).
-- m >>= pure gives:
--
--   pre  : P ⊓ (universe ∕ Q)
--        = P ⊓ universe           -- by R2
--        = universe ⊓ P           -- by C1
--        = P                       -- by C3
--
--   post : Q · empty = Q                      -- by S2
--
--   fut  : (F ∖ empty) ⊓ universe
--        = F ⊓ universe                       -- by L1
--        = universe ⊓ F                       -- by C1
--        = F                                  -- by C3
--
-- All three components equal those of m.                                       □
--
-- ── Law 3: associativity — (m >>= f) >>= g = m >>= (f >=> g) ─────────────────
--
-- Let m = (a,P,Q,F),  f a = (b,P',Q',F'),  g b = (c,P'',Q'',F'').
--
-- LHS: compute m >>= f first → (b, P⊓(P'∕Q), Q·Q', (F∖Q')⊓F'),
--      then bind g:
--
--   pre_L  = [P ⊓ (P' ∕ Q)] ⊓ (P'' ∕ (Q · Q'))
--   post_L = (Q · Q') · Q''
--   fut_L  = [((F ∖ Q') ⊓ F') ∖ Q''] ⊓ F''
--
-- RHS: compute f a >>= g first → (c, P'⊓(P''∕Q'), Q'·Q'', (F'∖Q'')⊓F''),
--      then bind that into m:
--
--   pre_R  = P ⊓ ((P' ⊓ (P'' ∕ Q')) ∕ Q)
--   post_R = Q · (Q' · Q'')
--   fut_R  = (F ∖ (Q' · Q'')) ⊓ ((F' ∖ Q'') ⊓ F'')
--
-- post: post_L = (Q · Q') · Q'' = Q · (Q' · Q'') = post_R              by S3  □
--
-- pre:  expand pre_R using ∕-distributivity then R3:
--   (P' ⊓ (P'' ∕ Q')) ∕ Q
--     = (P' ∕ Q) ⊓ ((P'' ∕ Q') ∕ Q)    by R4
--     = (P' ∕ Q) ⊓ (P'' ∕ (Q · Q'))    by R3
--   so pre_R = P ⊓ ((P' ∕ Q) ⊓ (P'' ∕ (Q · Q')))
--           = (P ⊓ (P' ∕ Q)) ⊓ (P'' ∕ (Q · Q')) = pre_L          by C2  □
--
-- fut:  expand fut_L using L4 then L3:
--   ((F ∖ Q') ⊓ F') ∖ Q''
--     = (F ∖ Q') ∖ Q''  ⊓  (F' ∖ Q'')  by L4
--     = F ∖ (Q' · Q'')  ⊓  (F' ∖ Q'')  by L3
--   so fut_L = ((F ∖ (Q'·Q'')) ⊓ (F' ∖ Q'')) ⊓ F''
--           = (F ∖ (Q'·Q'')) ⊓ ((F' ∖ Q'') ⊓ F'') = fut_R         by C2  □
