module Pledge.RE
    ( -- * Regular expressions
      RE(..)
    , top
    , globally
    , finally
    , never
    , noUntil
    , previously
      -- * Membership / derivatives
    , nullable
    , atoms
    , firstWith
    , first
    , derivative
    , antiDeriv
    , reSubtraction
    , normalize
      -- * LTL
    , LTL(..)
    , toSingleStep
    , ltlToRe
      -- * Display helper
    , printOfPledgeRE
    ) where

import Prelude hiding ((<>))
import Data.List (union, nub)
import Pledge.Core
import Pledge.Presburger

-- | Extended regular expressions over a typed event alphabet.
--
-- The type parameter @t@ is the payload type of 'Event'; the concrete type
-- used throughout the library is 'Term'.  Complement ('Not') is a first-class
-- constructor — no DFA construction is required because the Brzozowski
-- derivative commutes with complement: @∂_a(¬r) = ¬(∂_a(r))@.
data RE t
    = Bot              -- ^ ∅  — empty language
    | Epsilon          -- ^ ε  — empty word
    | Single (Event t) -- ^ {e} — single-event language ('Wildcard' matches anything)
    | Seq  (RE t) (RE t) -- ^ r₁ · r₂ — concatenation
    | Or   (RE t) (RE t) -- ^ r₁ ∪ r₂ — union
    | And  (RE t) (RE t) -- ^ r₁ ∩ r₂ — intersection
    | Star (RE t)        -- ^ r*  — Kleene star
    | Not  (RE t)        -- ^ ¬r  — complement (handled algebraically)

instance (Eq t) => Eq (RE t) where
    Bot == Bot = True
    Epsilon == Epsilon = True
    Single e1 == Single e2 = e1 == e2
    Seq r1a r2a == Seq r1b r2b = r1a == r1b && r2a == r2b
    Or r1a r2a == Or r1b r2b = r1a == r1b && r2a == r2b
    And r1a r2a == And r1b r2b = r1a == r1b && r2a == r2b
    Star r1 == Star r2 = r1 == r2
    Not r1 == Not r2 = r1 == r2
    _ == _ = False

instance (Show t) => Show (RE t) where
    show Bot         = "∅"
    show Epsilon     = "ε"
    show (Single e)  = show e
    -- top: ¬∅ = Σ*
    show (Not Bot)   = "Σ*"
    -- finally: Σ* · ev · Σ*  →  F(ev)
    show (Seq (Not Bot) (Seq (Single ev) (Not Bot))) =
        "F(" ++ show ev ++ ")"
    -- never: ¬F(ev)
    show (Not (Seq (Not Bot) (Seq (Single ev) (Not Bot)))) =
        "¬F(" ++ show ev ++ ")"
    -- noUntil(e, g): ¬((Σ\{g})* · e · Σ*)
    show (Not (Seq (Star (And (Single Wildcard) (Not (Single g)))) (Seq (Single e) (Not Bot)))) =
        "noUntil(" ++ show e ++ ", " ++ show g ++ ")"
    -- general cases
    show (Seq r1 r2) = show r1 ++ " · " ++ show r2
    show (Or  r1 r2) = "(" ++ show r1 ++ ") ∨ (" ++ show r2 ++ ")"
    show (And r1 r2) = "(" ++ show r1 ++ ") ∧ (" ++ show r2 ++ ")"
    show (Star r)    = "(" ++ show r ++ ")*"
    show (Not r)     = "¬(" ++ show r ++ ")"

-- | @Σ*@ — the universal language, complement of the empty language (@¬∅@).
top :: RE t
top = Not Bot

-- | @□ev@ — @ev@ must occur at every step: @ev*@.
globally :: Event t -> RE t
globally ev = Star (Single ev)

-- | @◇ev@ — @ev@ must occur at some future step: @Σ* · ev · Σ*@.
-- Use in @fut@ slots to express a deferred obligation.
finally :: Event t -> RE t
finally ev = Seq top (Seq (Single ev) top)

-- | @¬◇ev@ — @ev@ must /never/ occur: @¬(Σ* · ev · Σ*)@.
never :: Event t -> RE t
never ev = Not (finally ev)

-- | @noUntil e g@ — @e@ must not occur before @g@: @¬((Σ∖{g})* · e · Σ*)@.
-- Once @g@ has occurred, @e@ is unrestricted.
noUntil :: Event t -> Event t -> RE t
noUntil e g = Not (Seq (Star (And (Single Wildcard) (Not (Single g)))) (Seq (Single e) top))

-- | @previously ev@ is the past-facing alias for 'finally'.
-- Use it as a /pre/-condition to assert that @ev@ occurred somewhere in
-- the preceding trace.  The underlying RE is @Σ* · ev · Σ*@, identical to
-- @finally ev@: the name exists purely to signal intent at the call site —
-- write @previously ev@ in @pre@ slots and @finally ev@ in @fut@ slots.
previously :: Event t -> RE t
previously = finally

-- | @ν(r)@: returns @True@ iff @ε ∈ L(r)@ (the empty word is accepted).
nullable :: RE t -> Bool
nullable Bot          = False
nullable Epsilon      = True
nullable (Single _)   = False
nullable (Seq r1 r2)  = nullable r1 && nullable r2
nullable (Or  r1 r2)  = nullable r1 || nullable r2
nullable (And r1 r2)  = nullable r1 && nullable r2
nullable (Star _)     = True
nullable (Not r)      = not (nullable r)   -- ν(¬r) = ¬ν(r)

-- | Collect all concrete (non-'Wildcard') events mentioned in an 'RE'.
-- Forms the effective alphabet for complement unfolding in 'firstWith'.
atoms :: Eq t => RE t -> [Event t]
atoms Bot               = []
atoms Epsilon           = []
atoms (Single Wildcard) = []
atoms (Single e)        = [e]
atoms (Seq r1 r2)       = atoms r1 `union` atoms r2
atoms (Or  r1 r2)       = atoms r1 `union` atoms r2
atoms (And r1 r2)       = atoms r1 `union` atoms r2
atoms (Star r)          = atoms r
atoms (Not r)           = atoms r

-- | Events from @alph@ that can begin a word in @L(r)@.
-- For @Not r@: @e ∈ first(¬r)@ iff @∂_e(r) ≠ Σ*@, checked for every
-- event in the supplied alphabet.
firstWith :: Eq t => [Event t] -> RE t -> [Event t]
firstWith _    Bot               = []
firstWith _    Epsilon           = []
firstWith _    (Single e)        = [e]
firstWith alph (Seq r1 r2)
    | nullable r1                = firstWith alph r1 `union` firstWith alph r2
    | otherwise                  = firstWith alph r1
firstWith alph (Or  r1 r2)      = firstWith alph r1 `union` firstWith alph r2
firstWith alph (And r1 r2)      = [e | e <- firstWith alph r1, e `elem` firstWith alph r2]
firstWith alph (Star r)         = firstWith alph r
firstWith alph (Not r)          = [e | e <- alph, not (isTotal (normalize (derivative e r)))]
  where
    isTotal (Not Bot) = True
    isTotal _         = False

-- | Convenience wrapper around 'firstWith' that uses the events in @r@
-- itself as the alphabet.
first :: Eq t => RE t -> [Event t]
first r = firstWith (atoms r) r

-- | Brzozowski derivative @∂_e(r)@: the RE for all continuations after event @e@.
-- Key law for complement: @∂_a(¬r) = ¬(∂_a(r))@ — no DFA construction needed.
derivative :: Eq t => Event t -> RE t -> RE t
derivative _ Bot          = Bot
derivative _ Epsilon      = Bot
derivative e (Single p)   = if subsumesEvent e p then Epsilon else Bot
derivative e (Seq r1 r2)
    | nullable r1           = Or (Seq (derivative e r1) r2) (derivative e r2)
    | otherwise             = Seq (derivative e r1) r2
derivative e (Or  r1 r2)  = Or  (derivative e r1) (derivative e r2)
derivative e (And r1 r2)  = And (derivative e r1) (derivative e r2)
derivative e (Star r)     = Seq (derivative e r) (Star r)
derivative e (Not r)      = Not (derivative e r)   -- ∂_a(¬r) = ¬(∂_a(r))

-- | Antimirov partial derivatives @∂_e^A(r)@: a list of REs whose language
-- /union/ equals @L(∂_e(r))@.  Compared to the single Brzozowski derivative,
-- Antimirov splitting keeps individual terms smaller:
--
-- * @Or@ distributes into a union of smaller residuals.
-- * @Seq@ factors out the tail @r2@: @{t · r2 | t ∈ ∂_e^A(r1)}@, plus
--   the partial derivatives of @r2@ directly when @r1@ is nullable.
-- * @Star@ unfolds one step: @{t · r* | t ∈ ∂_e^A(r)}@.
-- * @And@ and @Not@ fall back to the unique Brzozowski derivative (singleton list).
antiDeriv :: Eq t => Event t -> RE t -> [RE t]
antiDeriv _ Bot           = []
antiDeriv _ Epsilon       = []
antiDeriv e (Single p)
    | subsumesEvent e p   = [Epsilon]
    | otherwise           = []
antiDeriv e (Or  r1 r2)   = nub (antiDeriv e r1 ++ antiDeriv e r2)
antiDeriv e (Seq r1 r2)   =
    let left  = map (\t -> normalize (Seq t r2)) (antiDeriv e r1)
        right = if nullable r1 then antiDeriv e r2 else []
    in nub (left ++ right)
antiDeriv e (Star r)      = map (\t -> normalize (Seq t (Star r))) (antiDeriv e r)
antiDeriv e (And r1 r2)   = [normalize (And (derivative e r1) (derivative e r2))]
antiDeriv e (Not r)       = [normalize (Not (derivative e r))]

-- | Left-quotient @r1 \\ r2@: the residual obligation in @r2@ after the
-- trace described by @r1@.
--
-- Computed via Antimirov partial derivatives on @r2@: instead of a single
-- Brzozowski step @∂_e(r2)@, the full Antimirov set @∂_e^A(r2)@ is used,
-- keeping intermediate terms smaller.  The result language is identical to
-- the Brzozowski version because @⋃ L(∂_e^A(r2)) = L(∂_e(r2))@.
reSubtraction :: Eq t => RE t -> RE t -> RE t
reSubtraction Epsilon r2 = r2
reSubtraction r1 r2 =
    let alph   = atoms r1 `union` atoms r2   -- combined alphabet for complement unfolding
        evts   = firstWith alph r1
        step e =
            let dr1  = normalize (derivative e r1)
                dr2s = antiDeriv e r2           -- Antimirov set of r2 residuals
            in foldr (Or . reSubtraction dr1) Bot dr2s
    in foldr (Or . step) Bot evts

-- | Linear Temporal Logic formulae over finite traces.
-- Use 'ltlToRe' to translate to 'RE'; no automaton construction is required.
data LTL t
    = LTLTrue
    | LTLFalse
    | LTLAtom     (Event t)
    | LTLNot      (LTL t)
    | LTLAnd      (LTL t) (LTL t)
    | LTLOr       (LTL t) (LTL t)
    | LTLNext     (LTL t)          -- ^ @X φ@  strong next
    | LTLUntil    (LTL t) (LTL t) -- ^ @φ U ψ@
    | LTLFinally  (LTL t)          -- ^ @F φ ≜ ⊤ U φ ≡ Σ* · ⟦φ⟧@
    | LTLGlobally (LTL t)          -- ^ @G φ ≜ ¬F¬φ ≡ ¬(Σ* · ¬⟦φ⟧)@


-- ── Single-step projection ────────────────────────────────────────────────────
-- toSingleStep l: the RE for a single event satisfying l at the current step.
-- This is the correct building block for LTLUntil:
--   ⟦φ U ψ⟧  =  toSingleStep(φ)* · ⟦ψ⟧
--
-- Using ltlToRe l1 directly would be wrong: ⟦l1⟧ may contain words of
-- length > 1 (e.g. LTLNext, LTLFinally), so Star ⟦l1⟧ iterates over
-- multi-event matches rather than individual steps.
--
-- Well-defined for propositional l (Boolean combinations of LTLAtom).
-- For temporal operators inside the Until left-hand side (LTLNext, LTLFinally,
-- LTLGlobally, nested LTLUntil) there is no single-step projection; Bot is
-- returned as a conservative error signal that makes the enclosing Until
-- unsatisfiable, surfacing the limitation rather than silently mis-specifying.
--
-- The LTLNot case intersects with Single Wildcard (Σ^1) to keep the result
-- length-1: bare Not (toSingleStep l) would include ε and multi-event words.

-- Returns Nothing for temporal operators (LTLNext, LTLUntil, LTLFinally,
-- LTLGlobally), which have no single-step projection.
toSingleStep :: LTL t -> Maybe (RE t)
toSingleStep LTLTrue         = Just (Single Wildcard)               -- any single event
toSingleStep LTLFalse        = Just Bot                             -- no event satisfies False
toSingleStep (LTLAtom e)     = Just (Single e)                      -- exactly event e
toSingleStep (LTLNot l)      = And (Single Wildcard) . Not          -- Σ^1 ∩ ¬step(l)
                                   <$> toSingleStep l
toSingleStep (LTLAnd l1 l2)  = And <$> toSingleStep l1 <*> toSingleStep l2
toSingleStep (LTLOr  l1 l2)  = Or  <$> toSingleStep l1 <*> toSingleStep l2
toSingleStep _               = Nothing  -- temporal operators not representable as a single step


-- | Algebraic translation @LTLf → RE@.  No automaton construction is needed;
-- complement is handled by the 'Not' constructor directly.
-- Returns 'Nothing' when 'LTLUntil'\'s left-hand side contains a temporal
-- operator with no single-step projection (see 'toSingleStep').
ltlToRe :: LTL t -> Maybe (RE t)
ltlToRe LTLTrue            = Just top                         -- ¬∅  = Σ*
ltlToRe LTLFalse           = Just Bot                              -- ∅
ltlToRe (LTLAtom e)        = Just (Single e)
ltlToRe (LTLNot l)         = Not <$> ltlToRe l                  -- ¬⟦l⟧
ltlToRe (LTLAnd l1 l2)     = And <$> ltlToRe l1 <*> ltlToRe l2  -- ⟦l1⟧ ∩ ⟦l2⟧
ltlToRe (LTLOr  l1 l2)     = Or  <$> ltlToRe l1 <*> ltlToRe l2  -- ⟦l1⟧ ∪ ⟦l2⟧
ltlToRe (LTLNext l)        = Seq (Single Wildcard) <$> ltlToRe l   -- Σ · ⟦l⟧
ltlToRe (LTLUntil l1 l2)   = Seq . Star <$> toSingleStep l1          -- step(l1)* · ⟦l2⟧
                                           <*> ltlToRe l2
ltlToRe (LTLFinally l)     = Seq top <$> ltlToRe l            -- Σ* · ⟦l⟧
ltlToRe (LTLGlobally l)    = Not . Seq top . Not                 -- ¬(Σ* · ¬⟦l⟧)
                                   <$> ltlToRe l

-- ── Composable RE instance ────────────────────────────────────────────────────

instance Eq t => Composable (RE t) where
    concatenation = Seq
    conjunction   = And
    empty         = Epsilon
    universe      = top
    subtraction   = reSubtraction

-- | Simplify an 'RE' using algebraic identities and De Morgan laws.
--
-- Key reductions: @∅ · r = ∅@, @ε · r = r@, @r ∪ r = r@, @¬¬r = r@,
-- @¬(r₁ ∨ r₂) = ¬r₁ ∧ ¬r₂@, @∅* = ε@, @ε* = ε@.
-- Does /not/ call 'derivative' internally, so it is always terminating.
normalize :: Eq t => RE t -> RE t
normalize r = case r of
    Seq r1 r2 -> case (normalize r1, normalize r2) of
        (Bot, _)      -> Bot
        (_, Bot)      -> Bot
        (Epsilon, r') -> r'
        (r', Epsilon) -> r'
        (r1', r2')    -> Seq r1' r2'

    Or r1 r2 -> case (normalize r1, normalize r2) of
        (Bot, r')        -> r'
        (r', Bot)        -> r'
        (r1', r2')
            | r1' == r2'  -> r1'
            | isTop r1'  -> top
            | isTop r2'  -> top
        (r1', r2')       -> Or r1' r2'

    And r1 r2 -> case (normalize r1, normalize r2) of
        (Bot, _)         -> Bot
        (_, Bot)         -> Bot
        (r1', r2')
            | r1' == r2'  -> r1'
            | isTop r1'  -> r2'
            | isTop r2'  -> r1'
        (Epsilon, r')    -> if nullable r' then Epsilon else Bot
        (r', Epsilon)    -> if nullable r' then Epsilon else Bot
        (r1', r2')       -> And r1' r2'

    -- Complement: involution + De Morgan laws
    Not r1 -> case normalize r1 of
        Not r'       -> r'                                    -- ¬¬r = r
        Or  r1' r2'  -> normalize (And (Not r1') (Not r2'))  -- De Morgan
        And r1' r2'  -> normalize (Or  (Not r1') (Not r2'))  -- De Morgan
        Bot          -> top                              -- ¬∅  = Σ*
        r' | isTop r' -> Bot                                -- ¬Σ* = ∅
        r'             -> Not r'

    Star r1 -> case normalize r1 of
        Bot     -> Epsilon   -- ∅* = ε
        Epsilon -> Epsilon   -- ε* = ε
        r'      -> Star r'

    _ -> r
  where
    isTop (Not Bot) = True
    isTop _         = False

-- | Run a 'Pledge' action once, print the four components, and return the
-- result.  Uses 'inspect' so the underlying @IO@ action executes exactly once.
printOfPledgeRE :: forall t a. (Show a, Show t, Eq t) => String -> Pledge IO (RE t) a -> IO a
printOfPledgeRE name prog = do
    PledgeResult r preC postC futC <- inspect prog
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Ret:    " ++ show r
    putStrLn $ "Pre:    " ++ show (normalize preC)
    putStrLn $ "Post:   " ++ show (normalize postC)
    putStrLn $ "Future: " ++ show (normalize futC)
    return r
