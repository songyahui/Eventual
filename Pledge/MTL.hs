module Pledge.MTL
    ( -- * Metric intervals
      Interval(..)
    , within
    , exactly
    , atLeast
    , fromTo
    , intervalSteps
      -- * Bounded metric temporal logic
    , MTL(..)
    , mtlToSingleStep
    , mtlToRe
      -- * Bounded RE combinators
    , reptSeq
    , sigmaPow
    , atMostSeq
      -- * Event-level bounded obligations
    , finallyWithin
    , neverWithin
    , globallyWithin
      -- * Display helper
    , printOfPledgeMTL
    ) where

import Pledge.Core
import Pledge.Event (Event(..))
import Pledge.RE

-- $overview
--
-- Discrete-time (step-metric) Metric Temporal Logic over finite traces.
-- Time is the position in the trace, so every metric modality is the
-- corresponding LTL$_f$ operator (see "Pledge.RE") with 'Star' replaced by
-- /bounded/ repetition.  A bounded @MTL@ formula still translates to a plain
-- finite 'RE', so the Brzozowski machinery — including the complement law
-- @∂_a(¬r) = ¬(∂_a r)@ — carries over unchanged; no automaton is built.
--
-- 'MTLFinally', 'MTLNext' and the propositional cases follow the LTL$_f$
-- convention of "Pledge.RE" verbatim (@⟦φ⟧@ is the language of trace
-- /suffixes/ satisfying @φ@), so @'atLeast' 0@ recovers the LTL translation
-- modulo normalization:
--
-- * @'mtlToRe' ('MTLFinally' ('atLeast' 0) φ)  ≡  ltlToRe (LTLFinally φ)@
-- * @'mtlToRe' ('MTLUntil'   ('atLeast' 0) φ ψ) ≡  ltlToRe (LTLUntil φ ψ)@
--
-- 'MTLGlobally' and 'MTLUntil' take a per-/step/ view of their propositional
-- argument ('mtlToSingleStep'), so @'mtlToRe'@ returns 'Nothing' when that
-- argument carries a temporal operator.

-- ── Metric intervals ─────────────────────────────────────────────────────────

-- | A metric constraint on step count: @'Interval' lo hi@ denotes @[lo, hi]@
-- when @hi@ is @'Just' b@ and @[lo, ∞)@ when @hi@ is @'Nothing'@.
--
-- @lo < 0@ is malformed and makes 'mtlToRe' return 'Nothing'.  An inverted
-- bound (@b < lo@) denotes the /empty/ interval: @F@ over it is @∅@ and @G@
-- over it is vacuously @Σ*@.
data Interval = Interval Int (Maybe Int)
    deriving (Eq, Show)

-- | @[0, n]@ — within @n@ steps (inclusive).
within :: Int -> Interval
within n = Interval 0 (Just n)

-- | @[n, n]@ — at exactly @n@ steps from now.
exactly :: Int -> Interval
exactly n = Interval n (Just n)

-- | @[n, ∞)@ — no sooner than @n@ steps from now.
atLeast :: Int -> Interval
atLeast n = Interval n Nothing

-- | @[lo, hi]@.
fromTo :: Int -> Int -> Interval
fromTo lo hi = Interval lo (Just hi)

-- | Expand an interval to the offsets it admits:
--
-- * @'Just' ('Left' a)@   — the unbounded tail @[a, ∞)@;
-- * @'Just' ('Right' ks)@ — the finite offset list @[lo .. hi]@
--   (empty when @hi < lo@);
-- * @'Nothing'@           — @lo < 0@.
intervalSteps :: Interval -> Maybe (Either Int [Int])
intervalSteps (Interval lo hi)
    | lo < 0    = Nothing
    | otherwise = case hi of
        Nothing -> Just (Left lo)
        Just b  -> Just (Right [lo .. b])

-- ── Bounded RE combinators ───────────────────────────────────────────────────

-- | @reptSeq r n = r · r · … · r@ (@n@ copies); @ε@ for @n <= 0@.
reptSeq :: RE t -> Int -> RE t
reptSeq _ n | n <= 0 = Epsilon
reptSeq r n          = Seq r (reptSeq r (n - 1))

-- | @sigmaPow n = Σ^n@ — any @n@ events.
sigmaPow :: Int -> RE t
sigmaPow = reptSeq (Single Wildcard)

-- | @atMostSeq r n = ⋃_{k=0}^{n} r^k@ — bounded (at-most-@n@) repetition.
atMostSeq :: RE t -> Int -> RE t
atMostSeq r n = foldr Or Bot [ reptSeq r k | k <- [0 .. max 0 n] ]

-- ── Bounded metric temporal logic ───────────────────────────────────────────

-- | Metric temporal logic formulae with a step metric, over finite traces.
--
-- The first block are the logical / temporal constructors; 'mtlToRe'
-- translates them to 'RE'.  The second block (@MTLEmpty@, @MTLSeq@,
-- @MTLLQuot@, @MTLRQuot@) is the /free/ 'Composable' algebra — the operators
-- a 'Pledge' bind needs on its specification component.  They have no
-- independent meaning; 'mtlToRe' interprets them straight into the
-- corresponding 'RE' operation.  @'universe'@ and @'conjunction'@ reuse
-- 'MTLTrue' and 'MTLAnd'.
data MTL t
    = MTLTrue
    | MTLFalse
    | MTLAtom     (Event t)
    | MTLNot      (MTL t)
    | MTLAnd      (MTL t) (MTL t)
    | MTLOr       (MTL t) (MTL t)
    | MTLNext     (MTL t)                    -- ^ @X φ@ ≜ @F_[1,1] φ@ (strong next)
    | MTLUntil    Interval (MTL t) (MTL t)  -- ^ @φ U_I ψ@ (@φ@ propositional)
    | MTLFinally  Interval (MTL t)           -- ^ @F_I φ ≜ ⊤ U_I φ@
    | MTLGlobally Interval (MTL t)           -- ^ @G_I φ@ (@φ@ propositional)
      -- Free 'Composable' algebra (interpreted by 'mtlToRe'):
    | MTLEmpty                               -- ^ @ε@ — 'empty'
    | MTLSeq      (MTL t) (MTL t)           -- ^ @·@ — 'concatenation'
    | MTLLQuot    (MTL t) (MTL t)           -- ^ 'leftQuotient'  (@∖@)
    | MTLRQuot    (MTL t) (MTL t)           -- ^ 'rightQuotient' (@∕@)
    deriving (Eq, Show)

-- | Single-event RE for a formula satisfied at the current step.
-- Defined for propositional @φ@ only; 'Nothing' for temporal and algebra
-- constructors, which have no single-step projection (cf. 'toSingleStep' in
-- "Pledge.RE").
mtlToSingleStep :: MTL t -> Maybe (RE t)
mtlToSingleStep MTLTrue        = Just (Single Wildcard)
mtlToSingleStep MTLFalse       = Just Bot
mtlToSingleStep (MTLAtom e)    = Just (Single e)
mtlToSingleStep (MTLNot p)     = And (Single Wildcard) . Not <$> mtlToSingleStep p
mtlToSingleStep (MTLAnd p q)   = And <$> mtlToSingleStep p <*> mtlToSingleStep q
mtlToSingleStep (MTLOr  p q)   = Or  <$> mtlToSingleStep p <*> mtlToSingleStep q
mtlToSingleStep _              = Nothing

-- | Algebraic translation @MTL → RE@.  Follows the LTL$_f$ mapping of
-- "Pledge.RE" (@⟦φ⟧@ is the language of trace /suffixes/ satisfying @φ@),
-- with bounded repetition in place of 'Star':
--
-- @
--   ⟦F_[a,b] φ⟧   = ⋃_{k=a}^{b} Σ^k · ⟦φ⟧
--   ⟦F_[a,∞) φ⟧   = Σ^a · Σ* · ⟦φ⟧
--   ⟦G_[a,b] φ⟧   = ¬( ⋃_{k=a}^{b} Σ^k · (Σ ∩ ¬step(φ)) · Σ* )
--   ⟦G_[a,∞) φ⟧   = ¬( Σ^a · Σ* · (Σ ∩ ¬step(φ)) · Σ* )
--   ⟦φ U_[a,b] ψ⟧ = ⋃_{k=a}^{b} step(φ)^k · ⟦ψ⟧
--   ⟦φ U_[a,∞) ψ⟧ = step(φ)^a · step(φ)* · ⟦ψ⟧
-- @
--
-- The free-algebra constructors interpret directly:
-- @⟦MTLEmpty⟧ = ε@, @⟦MTLSeq p q⟧ = ⟦p⟧·⟦q⟧@,
-- @⟦MTLLQuot p q⟧ = 'reLeftQuotient' ⟦p⟧ ⟦q⟧@, and dually for @MTLRQuot@.
--
-- Returns 'Nothing' when an interval has @lo < 0@, or when the propositional
-- argument of 'MTLGlobally' \/ 'MTLUntil' carries a temporal operator with no
-- single-step projection (see 'mtlToSingleStep').
--
-- (@'Eq' t@ is required only for the quotient constructors, which run the
-- Brzozowski worklist of 'reLeftQuotient'.)
mtlToRe :: Eq t => MTL t -> Maybe (RE t)
mtlToRe MTLTrue           = Just top
mtlToRe MTLFalse          = Just Bot
mtlToRe (MTLAtom e)       = Just (Single e)
mtlToRe (MTLNot p)        = Not <$> mtlToRe p
mtlToRe (MTLAnd p q)      = And <$> mtlToRe p <*> mtlToRe q
mtlToRe (MTLOr  p q)      = Or  <$> mtlToRe p <*> mtlToRe q
mtlToRe (MTLNext p)       = Seq (Single Wildcard) <$> mtlToRe p   -- Σ · ⟦φ⟧
mtlToRe (MTLFinally i p)  = do
    ks <- intervalSteps i
    r  <- mtlToRe p
    pure $ case ks of
        Left  a  -> Seq (sigmaPow a) (Seq top r)                  -- Σ^a · Σ* · ⟦φ⟧
        Right xs -> foldr Or Bot [ Seq (sigmaPow k) r | k <- xs ] -- ⋃ Σ^k · ⟦φ⟧
mtlToRe (MTLGlobally i p) = do
    ks <- intervalSteps i
    s  <- mtlToSingleStep p
    let violate = Seq (And (Single Wildcard) (Not s)) top          -- (Σ ∩ ¬step(φ)) · Σ*
    pure . Not $ case ks of
        Left  a  -> Seq (sigmaPow a) (Seq top violate)             -- Σ^a · Σ* · violate
        Right xs -> foldr Or Bot [ Seq (sigmaPow k) violate | k <- xs ]
mtlToRe (MTLUntil i p q)  = do
    ks <- intervalSteps i
    s  <- mtlToSingleStep p
    r  <- mtlToRe q
    pure $ case ks of
        Left  a  -> Seq (reptSeq s a) (Seq (Star s) r)            -- step(φ)^a · step(φ)* · ⟦ψ⟧
        Right xs -> foldr Or Bot [ Seq (reptSeq s k) r | k <- xs ]
mtlToRe MTLEmpty          = Just Epsilon
mtlToRe (MTLSeq p q)      = Seq            <$> mtlToRe p <*> mtlToRe q
mtlToRe (MTLLQuot p q)    = reLeftQuotient  <$> mtlToRe p <*> mtlToRe q
mtlToRe (MTLRQuot p q)    = reRightQuotient <$> mtlToRe p <*> mtlToRe q

-- ── Composable instance ─────────────────────────────────────────────────────
--
-- The free algebra: every operation is its constructor, and 'mtlToRe' is the
-- (partial) interpretation homomorphism into 'RE'.  The 'Composable' laws
-- (S1–S3, C1–C3, L1–L4, R1–R4 of "Pledge.Core") therefore hold /under
-- 'mtlToRe'/ — as language equalities — exactly as they hold for the 'RE'
-- instance under 'normalize'.  They are not syntactic: @'concatenation'
-- 'empty' a@ is @MTLSeq MTLEmpty a@, not @a@.  Discharge checks on a 'Pledge'
-- component must go through @'normalize' . 'mtlToRe'@ (see 'printOfPledgeMTL').

instance Eq t => Composable (MTL t) where
    concatenation = MTLSeq
    conjunction   = MTLAnd
    empty         = MTLEmpty
    universe      = MTLTrue
    leftQuotient  = MTLLQuot
    rightQuotient = MTLRQuot

-- ── Event-level bounded obligations ─────────────────────────────────────────
-- These mirror 'finally' / 'never' / 'globally' in "Pledge.RE": each carries a
-- trailing @Σ*@ so it reads as "at some step", not "the trace ends there".
-- They are the forms meant for @fut@ / @pre@ slots of a 'Pledge'.

-- | @F_[0,n] ev@ as a deferred obligation: @ev@ must occur within @n@ steps.
-- @⋃_{k=0}^{n} Σ^k · ev · Σ*@.  In a @fut@ slot the compiled residual
-- normalizes to @∅@ once @n@ steps pass without @ev@ — a deadline
-- /violation/, not a pending obligation.
finallyWithin :: Int -> Event t -> MTL t
finallyWithin n ev = MTLFinally (within n) (MTLSeq (MTLAtom ev) MTLTrue)

-- | @ev@ must not occur during the first @n@ steps (unrestricted afterwards):
-- @¬(⋃_{k=0}^{n-1} Σ^k · ev · Σ*)@.  Vacuous when @n <= 0@.
neverWithin :: Int -> Event t -> MTL t
neverWithin n ev = MTLNot (MTLFinally (fromTo 0 (n - 1)) (MTLSeq (MTLAtom ev) MTLTrue))

-- | @ev@ must occur at every one of the first @n@ steps (shorter traces
-- satisfy it vacuously): @¬(⋃_{k=0}^{n-1} Σ^k · (Σ ∩ ¬ev) · Σ*)@.
globallyWithin :: Int -> Event t -> MTL t
globallyWithin n ev = MTLGlobally (fromTo 0 (n - 1)) (MTLAtom ev)

-- ── Display helper ─────────────────────────────────────────────────────────

-- | Run a @'Pledge' IO ('MTL' t)@ action once (via 'inspect', so @IO@ fires
-- exactly once), then print each specification component as its
-- 'normalize'-d 'RE' compilation.  A component whose interval is malformed
-- prints @\<no RE translation>@.
printOfPledgeMTL :: (Show a, Show t, Eq t)
                 => String -> Pledge IO (MTL t) a -> IO a
printOfPledgeMTL name prog = do
    PledgeResult r preC postC futC <- inspect prog
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Ret:    " ++ show r
    putStrLn $ "Pre:    " ++ showComp preC
    putStrLn $ "Post:   " ++ showComp postC
    putStrLn $ "Future: " ++ showComp futC
    return r
  where
    showComp c = maybe "<no RE translation>" (show . normalize) (mtlToRe c)
