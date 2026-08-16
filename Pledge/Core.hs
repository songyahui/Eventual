module Pledge.Core
    ( -- * Composable class
      Composable(..)
    , (·)
    , (/\)
    , (\\)
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
-- An instance of @Composable@ must satisfy eight laws (see source comments
-- on the 'Pledge' monad instance for the full list):
--
-- * @'concatenation'@ is associative with unit @'empty'@.
-- * @'conjunction'@ is associative and commutative with unit @'universe'@.
-- * @'subtraction' p q@ computes the left-quotient of @q@ by @p@:
--   the residual obligation in @q@ not already discharged by @p@.
class Composable a where
    -- | Sequential composition.  Corresponds to RE concatenation, separating
    -- conjunction @*@ in SL, and @⊗@ in weighted REs.
    concatenation :: a -> a -> a
    -- | Simultaneous constraint.  Corresponds to RE intersection, ordinary
    -- conjunction @∧@ in SL, and pointwise @⊗@ in weighted REs.
    conjunction   :: a -> a -> a
    -- | Identity for 'concatenation'.  Corresponds to @ε@ (empty word) in RE,
    -- @emp@ in SL, and @WEps sone@ in weighted REs.
    empty         :: a
    -- | Identity for 'conjunction'.  Corresponds to @Σ*@ in RE, @⊤@ in SL,
    -- and @WStar (WSingle sone Wildcard)@ in weighted REs.
    universe      :: a
    -- | Left-quotient / residual: @subtraction post pre@ returns the part of
    -- @pre@ not already discharged by @post@.  Corresponds to the Brzozowski
    -- quotient in RE, the magic wand @-*@ in SL, and the weighted quotient in WRE.
    --
    -- Note the argument order: @post \\\\ pre@ reads as \"pre after post\".
    subtraction   :: a -> a -> a

-- | Product of two 'Composable' algebras: each operation is applied
-- component-wise.  Enables e.g. @'Pledge' IO (RE Term, WRE Prob Term) a@
-- for simultaneous Boolean and probabilistic reasoning.
instance (Composable a, Composable b) => Composable (a, b) where
    concatenation (a1, b1) (a2, b2) = (concatenation a1 a2, concatenation b1 b2)
    conjunction   (a1, b1) (a2, b2) = (conjunction   a1 a2, conjunction   b1 b2)
    subtraction   (a1, b1) (a2, b2) = (subtraction   a1 a2, subtraction   b1 b2)
    empty                           = (empty, empty)
    universe                        = (universe, universe)

-- | Lifts all 'Composable' operations through any 'Applicative' @m@,
-- so that @('·')@, @('/\\')@, and @('\\\\')@ work directly on @m eff@ values.
instance {-# OVERLAPPABLE #-} (Composable eff, Applicative m) => Composable (m eff) where
    concatenation = liftA2 concatenation
    conjunction   = liftA2 conjunction
    subtraction   = liftA2 subtraction
    empty         = pure empty
    universe      = pure universe

infixl 6 ·
-- | Infix alias for 'concatenation' (@infixl 6@).
(·) :: Composable a => a -> a -> a
(·) = concatenation

infixl 7 /\
-- | Infix alias for 'conjunction' (@infixl 7@).
(/\) :: Composable a => a -> a -> a
(/\) = conjunction

infixl 5 \\
-- | Left-quotient operator (@infixl 5@): @post \\\\ pre@ returns the residual
-- of @pre@ not discharged by @post@.
-- Note the reversed argument order relative to 'subtraction':
-- @a \\\\ b = subtraction b a@.
(\\) :: Composable a => a -> a -> a
a \\ b = subtraction b a

-- ── Pledge monad ─────────────────────────────────────────────────────────────

-- | A monadic action in @m@ augmented with three temporal specifications in @eff@.
--
-- @'Pledge' m eff a@ wraps a single @m@-action that produces all four
-- components at once:
--
-- @
-- (ret, pre, post, fut) :: (a, eff, eff, eff)
-- @
--
-- * @ret@  — the return value.
-- * @pre@  — precondition: what must have held in the preceding trace.
-- * @post@ — postcondition: what this action emits\/produces.
-- * @fut@  — future obligation: what must be discharged by subsequent actions.
--
-- Because all four components come from a single run of the underlying @m@
-- action, stateful resources (file handles, heap addresses, …) are allocated
-- exactly once per invocation.  Data-dependent future obligations arise
-- naturally: @ret@ is in scope when the programmer constructs @fut@ inside
-- the @m@ action.
--
-- 'Pledge' is a 'Monad' when @eff@ is 'Composable' and @m@ is a 'Monad'.
-- The bind rule propagates all four components automatically:
--
-- @
-- pre  (p >>= g)  =  pre p  \/\\  (pre (g _)  \\\\  post p)
-- post (p >>= g)  =  post p  ·   post (g _)
-- fut  (p >>= g)  =  (fut p  \\\\  post (g _))  \/\\  fut (g _)
-- @
newtype Pledge m eff a = Pledge { runPledge :: m (a, eff, eff, eff) }
-- Fields: ret, pre, post, fut

-- | Embed a plain @m@ action into 'Pledge' with trivial temporal conditions:
-- precondition @universe@ (trivially satisfied), postcondition @empty@
-- (emits nothing), future condition @universe@ (no future obligation).
liftPledge :: (Composable eff, Applicative m) => m a -> Pledge m eff a
liftPledge ma = Pledge $ fmap (, universe, empty, universe) ma

-- | All four components of a completed 'Pledge' action, collected in one run.
data PledgeResult eff a = PledgeResult
    { ret  :: a    -- ^ the return value
    , pre  :: eff  -- ^ precondition (what must have held before)
    , post :: eff  -- ^ postcondition (what this action emitted)
    , fut  :: eff  -- ^ future condition (what must still hold after)
    }

-- | Run a 'Pledge' action exactly once and collect all four components.
-- Prefer this over 'getRet' \/ 'getPre' \/ 'getPost' \/ 'getFut' whenever
-- @m@ has observable side effects (e.g. 'IO'), because each of those
-- helpers calls 'runPledge' separately.
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
        return (f x, preF /\ (preX \\ postF), postF · postX, (futF \\ postX) /\ futX)

instance (Composable eff, Monad m) => Monad (Pledge m eff) where
    return = pure
    Pledge ma >>= g = Pledge $ do
        (a, preA, postA, futA) <- ma
        (b, preB, postB, futB) <- runPledge (g a)
        return (b, preA /\ (preB \\ postA), postA · postB, (futA \\ postB) /\ futB)

-- ── Monad law proofs ──────────────────────────────────────────────────────────
--
-- Notation: write a Pledge as (ret, pre, post, fut).
--   pure x          = (x, universe, empty, universe)
--   (P,Q,F) >>= g   -- where g ret = (ret', P', Q', F')
--     = (ret', P /\ (P' \\ Q),  Q · Q',  (F \\ Q') /\ F')
--
-- The proofs require 'Composable' to satisfy these eight laws:
--
--   (C1)  empty · a       = a           left  identity of (·)
--   (C2)  a · empty       = a           right identity of (·)
--   (C3)  (a · b) · c     = a · (b · c) associativity  of (·)
--   (C4)  universe /\ a   = a           universe is identity of (/\)
--   (C5)  a \\ empty      = a           empty post discharges nothing
--   (C6)  universe \\ a   = universe    universe is stable under subtraction
--   (C7)  x \\ (a · b)    = (x \\ b) \\ a   sequential residual
--   (C8)  (a /\ b) \\ c   = (a \\ c) /\ (b \\ c)  subtraction distributes over (/\)
--
-- ── Law 1: left identity — pure a >>= f = f a ─────────────────────────────────
--
-- pure a = (a, universe, empty, universe).
-- Let f a = (b, P', Q', F').  Then pure a >>= f gives:
--
--   pre  : universe /\ (P' \\ empty)
--        = universe /\ P'             -- by C5: P' \\ empty = P'
--        = P'                         -- by C4: universe /\ P' = P'
--
--   post : empty · Q'
--        = Q'                         -- by C1
--
--   fut  : (universe \\ Q') /\ F'
--        = universe /\ F'             -- by C6: universe \\ Q' = universe
--        = F'                         -- by C4
--
-- All three components equal those of f a.                                    □
--
-- ── Law 2: right identity — m >>= pure = m ────────────────────────────────────
--
-- Let m = (a, P, Q, F).  pure a = (a, universe, empty, universe).
-- m >>= pure gives:
--
--   pre  : P /\ (universe \\ Q)
--        = P /\ universe              -- by C6
--        = P                          -- by C4
--
--   post : Q · empty
--        = Q                          -- by C2
--
--   fut  : (F \\ empty) /\ universe
--        = F /\ universe              -- by C5
--        = F                          -- by C4
--
-- All three components equal those of m.                                       □
--
-- ── Law 3: associativity — (m >>= f) >>= g = m >>= (f >=> g) ─────────────────
--
-- Let m = (a,P,Q,F),  f a = (b,P',Q',F'),  g b = (c,P'',Q'',F'').
--
-- LHS: compute m >>= f first → (b, P∧(P'\\Q), Q·Q', (F\\Q')∧F'),
--      then bind g:
--
--   pre_L  = [P /\ (P' \\ Q)] /\ (P'' \\ (Q · Q'))
--   post_L = (Q · Q') · Q''
--   fut_L  = [((F \\ Q') /\ F') \\ Q''] /\ F''
--
-- RHS: compute f a >>= g first → (c, P'∧(P''\\Q'), Q'·Q'', (F'\\Q'')∧F''),
--      then bind that into m:
--
--   pre_R  = P /\ ((P' /\ (P'' \\ Q')) \\ Q)
--   post_R = Q · (Q' · Q'')
--   fut_R  = (F \\ (Q' · Q'')) /\ ((F' \\ Q'') /\ F'')
--
-- post: post_L = (Q · Q') · Q'' = Q · (Q' · Q'') = post_R              by C3  □
--
-- pre:  expand pre_R using C8 then C7:
--   (P' /\ (P'' \\ Q')) \\ Q
--     = (P' \\ Q) /\ ((P'' \\ Q') \\ Q)    by C8
--     = (P' \\ Q) /\ (P'' \\ (Q · Q'))     by C7
--   so pre_R = P /\ (P' \\ Q) /\ (P'' \\ (Q · Q')) = pre_L             by C3  □
--
-- fut:  expand fut_L using C8 then C7:
--   ((F \\ Q') /\ F') \\ Q''
--     = (F \\ Q') \\ Q''  /\  (F' \\ Q'')  by C8
--     = F \\ (Q' · Q'')   /\  (F' \\ Q'')  by C7
--   so fut_L = (F \\ (Q'·Q'')) /\ (F' \\ Q'') /\ F'' = fut_R           by C4  □
