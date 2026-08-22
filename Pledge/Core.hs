module Pledge.Core
    ( -- * Composable class
      Composable(..)
    , (·)
    , (/\)
    , (\\)
    , (⊖)
      -- * Pledge monad
    , Pledge(..)
    , liftPledge
      -- * Safe inspection (run once)
    , PledgeResult(..)
    , inspect
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
-- * @'subtraction' p q@ is the left-quotient of @q@ by @p@.
-- * @'rev'@ is an involution, anti-homomorphism over @'concatenation'@,
--   and homomorphism over @'conjunction'@.
class Composable a where
    -- | Sequential composition: RE concatenation, @*@ in SL, @⊗@ in WRE.
    concatenation :: a -> a -> a
    -- | Simultaneous constraint: RE intersection, @∧@ in SL, pointwise @⊗@ in WRE.
    conjunction   :: a -> a -> a
    -- | Identity for 'concatenation': @ε@ in RE, @emp@ in SL.
    empty         :: a
    -- | Identity for 'conjunction': @Σ*@ in RE, @⊤@ in SL.
    universe      :: a
    -- | Left-quotient: @subtraction post pre@ is the residual of @pre@ not
    -- discharged by @post@.  Infix alias: @post \\\\ pre@ (see '(\\\\)').
    subtraction   :: a -> a -> a
    -- | Reversal involution: anti-homomorphism over 'concatenation'
    -- (@rev (a · b) = rev b · rev a@), homomorphism over 'conjunction'.
    -- Used to define '(⊖)'; full laws in the monad proof comments.
    rev           :: a -> a

-- | Component-wise 'Composable' instance for pairs.
instance (Composable a, Composable b) => Composable (a, b) where
    concatenation (a1, b1) (a2, b2) = (concatenation a1 a2, concatenation b1 b2)
    conjunction   (a1, b1) (a2, b2) = (conjunction   a1 a2, conjunction   b1 b2)
    subtraction   (a1, b1) (a2, b2) = (subtraction   a1 a2, subtraction   b1 b2)
    empty                           = (empty, empty)
    universe                        = (universe, universe)
    rev (a, b)                  = (rev a, rev b)

-- | Lifts 'Composable' pointwise through any 'Applicative'.
instance {-# OVERLAPPABLE #-} (Composable eff, Applicative m) => Composable (m eff) where
    concatenation = liftA2 concatenation
    conjunction   = liftA2 conjunction
    subtraction   = liftA2 subtraction
    empty         = pure empty
    universe      = pure universe
    rev       = fmap rev

infixl 6 ·
-- | Infix alias for 'concatenation' (@infixl 6@).
(·) :: Composable a => a -> a -> a
(·) = concatenation

infixl 7 /\
-- | Infix alias for 'conjunction' (@infixl 7@).
(/\) :: Composable a => a -> a -> a
(/\) = conjunction

infixl 5 \\
-- | Left-quotient (@infixl 5@): @post \\\\ pre@ is the residual of @pre@
-- not discharged by @post@.  Defined as @a \\\\ b = subtraction b a@.
(\\) :: Composable a => a -> a -> a
a \\ b = subtraction b a

infixl 5 ⊖
-- | Pre-residual (@infixl 5@): @P '⊖' Q = rev (rev P '\\\\' rev Q)@.
-- Satisfies the right-quotient law @(x '⊖' b) '⊖' a = x '⊖' (a '·' b)@
-- (see monad proof comments).  Used to compute preconditions in '>>='.
(⊖) :: Composable a => a -> a -> a
p ⊖ q = rev (rev p \\ rev q)

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
--
-- 'Pledge' is a 'Monad' when @eff@ is 'Composable' and @m@ is a 'Monad'.
-- The bind rule is:
--
-- @
-- pre  (p >>= g)  =  pre p  \/\  (pre (g _)  ⊖  post p)
-- post (p >>= g)  =  post p  ·   post (g _)
-- fut  (p >>= g)  =  (fut p  \\\\  post (g _))  \/\  fut (g _)
-- @
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
        return (f x, preF /\ (preX ⊖ postF), postF · postX, (futF \\ postX) /\ futX)

instance (Composable eff, Monad m) => Monad (Pledge m eff) where
    return = pure
    Pledge ma >>= g = Pledge $ do
        (a, preA, postA, futA) <- ma
        (b, preB, postB, futB) <- runPledge (g a)
        return (b, preA /\ (preB ⊖ postA), postA · postB, (futA \\ postB) /\ futB)

-- ── Monad law proofs ──────────────────────────────────────────────────────────
--
-- Notation: write a Pledge as (ret, pre, post, fut).
-- Abbreviate  P ⊖ Q  for  rev (rev P \\ rev Q)  (the pre-residual).
--
--   pure x          = (x, universe, empty, universe)
--   (P,Q,F) >>= g   -- where g _ = (ret', P', Q', F')
--     = (ret', P /\ (P' ⊖ Q),  Q · Q',  (F \\ Q') /\ F')
--
-- The proofs require 'Composable' to satisfy, for (·), (/\), (\\):
--
--   (C1)  empty · a            = a                    left  identity of (·)
--   (C2)  a · empty            = a                    right identity of (·)
--   (C3)  (a · b) · c          = a · (b · c)          associativity  of (·)
--   (C4)  universe /\ a        = a /\ universe = a    two-sided identity of (/\)
--   (C5)  a \\ empty           = a                    empty post discharges nothing
--   (C6)  universe \\ a        = universe             universe is stable under \\
--   (C7)  x \\ (a · b)         = (x \\ a) \\ b        left-quotient sequential law
--   (C8)  (a /\ b) \\ c        = (a \\ c) /\ (b \\ c) \\ distributes over (/\)
--
-- and for 'rev':
--
--   (Cr1) rev (rev x)          = x                    involution
--   (Cr2) rev (a · b)          = rev b · rev a         anti-homomorphism for (·)
--   (Cr3) rev (a /\ b)         = rev a /\ rev b        homomorphism for (/\)
--   (Cr4) rev empty            = empty
--   (Cr5) rev universe         = universe
--
-- Key derived law for ⊖ (proved from C7 + Cr1–Cr5):
--
--   (D3)  (x ⊖ b) ⊖ a = x ⊖ (a · b)                  right-quotient law for ⊖
--
--   Proof:  (x ⊖ b) ⊖ a
--         = rev ((rev x \\ rev b) \\ rev a)   -- Cr1
--         = rev (rev x \\ (rev b · rev a))    -- C7
--         = rev (rev x \\ rev (a · b))        -- Cr2
--         = x ⊖ (a · b)                       □
--
--   (\\) distributes over (/\) via C8; ⊖ distributes over (/\) analogously via C8 + Cr3.
--
-- ── Law 1: left identity — pure a >>= f = f a ─────────────────────────────────
--
-- pure a = (a, universe, empty, universe).
-- Let f a = (b, P', Q', F').  Then pure a >>= f gives:
--
--   pre  : universe /\ (P' ⊖ empty)
--        = universe /\ P'    -- P' ⊖ empty = P' by Cr4, C5, Cr1
--        = P'                -- by C4
--
--   post : empty · Q' = Q'                   -- by C1
--
--   fut  : (universe \\ Q') /\ F'
--        = universe /\ F'                     -- by C6
--        = F'                                 -- by C4
--
-- All three components equal those of f a.                                    □
--
-- ── Law 2: right identity — m >>= pure = m ────────────────────────────────────
--
-- Let m = (a, P, Q, F).  pure a = (a, universe, empty, universe).
-- m >>= pure gives:
--
--   pre  : P /\ (universe ⊖ Q)
--        = P /\ universe     -- universe ⊖ Q = universe by Cr5, C6, Cr5
--        = P                 -- by C4
--
--   post : Q · empty = Q                      -- by C2
--
--   fut  : (F \\ empty) /\ universe
--        = F /\ universe                      -- by C5
--        = F                                  -- by C4
--
-- All three components equal those of m.                                       □
--
-- ── Law 3: associativity — (m >>= f) >>= g = m >>= (f >=> g) ─────────────────
--
-- Let m = (a,P,Q,F),  f a = (b,P',Q',F'),  g b = (c,P'',Q'',F'').
--
-- LHS: compute m >>= f first → (b, P∧(P'⊖Q), Q·Q', (F\\Q')∧F'),
--      then bind g:
--
--   pre_L  = [P /\ (P' ⊖ Q)] /\ (P'' ⊖ (Q · Q'))
--   post_L = (Q · Q') · Q''
--   fut_L  = [((F \\ Q') /\ F') \\ Q''] /\ F''
--
-- RHS: compute f a >>= g first → (c, P'∧(P''⊖Q'), Q'·Q'', (F'\\Q'')∧F''),
--      then bind that into m:
--
--   pre_R  = P /\ ((P' /\ (P'' ⊖ Q')) ⊖ Q)
--   post_R = Q · (Q' · Q'')
--   fut_R  = (F \\ (Q' · Q'')) /\ ((F' \\ Q'') /\ F'')
--
-- post: post_L = (Q · Q') · Q'' = Q · (Q' · Q'') = post_R              by C3  □
--
-- pre:  expand pre_R using ⊖-distributivity then D3:
--   (P' /\ (P'' ⊖ Q')) ⊖ Q
--     = (P' ⊖ Q) /\ ((P'' ⊖ Q') ⊖ Q)    by ⊖ distributes over /\ (from C8 + Cr3)
--     = (P' ⊖ Q) /\ (P'' ⊖ (Q · Q'))    by D3
--   so pre_R = P /\ (P' ⊖ Q) /\ (P'' ⊖ (Q · Q')) = pre_L              □
--
-- fut:  expand fut_L using C8 then C7:
--   ((F \\ Q') /\ F') \\ Q''
--     = (F \\ Q') \\ Q''  /\  (F' \\ Q'')  by C8
--     = F \\ (Q' · Q'')   /\  (F' \\ Q'')  by C7
--   so fut_L = (F \\ (Q'·Q'')) /\ (F' \\ Q'') /\ F'' = fut_R           □
