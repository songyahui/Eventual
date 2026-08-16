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
      -- * Tests
    , langEq
    , prop_concat_assoc
    , prop_concat_left_id
    , prop_concat_right_id
    , prop_conj_assoc
    , prop_conj_comm
    , prop_conj_id
    , prop_sub_right_zero
    , prop_sub_universe
    , prop_sub_seq_dist
    , prop_sub_conj_dist
    , runRELawTests
    , printOfPledgeRE
    ) where

import Prelude hiding ((<>))
import Control.Monad (replicateM)
import Test.QuickCheck
import Data.List (union, nub)
import Pledge.Core
import Pledge.Presburger

-- ── Regular Expressions ───────────────────────────────────────────────────────

data RE t
    = Bot          -- ∅         empty language
    | Epsilon      -- ε         empty word
    | Single (Event t) -- a         single-event pattern
    | Seq  (RE t) (RE t)    -- r₁ · r₂   concatenation
    | Or   (RE t) (RE t)    -- r₁ + r₂   union
    | And  (RE t) (RE t)    -- r₁ ∩ r₂   intersection  (= ¬(¬r₁ + ¬r₂))
    | Star (RE t)      -- r*        Kleene star
    | Not  (RE t)       -- ¬r        complement

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

-- Shortcuts for common patterns:
-- Σ* — universal language, complement of the empty language
top :: RE t
top = Not Bot

-- G ev  — ev must occur at every step:  ev*
globally :: Event t -> RE t
globally ev = Star (Single ev)

-- F ev  — ev must occur at some step:  Σ* · ev · Σ*
finally :: Event t -> RE t
finally ev = Seq top (Seq (Single ev) top)

-- ¬F ev — ev must never occur again:  ¬(Σ* · ev · Σ*)
never :: Event t -> RE t
never ev = Not (finally ev)

-- noUntil e g: e must not occur before g.  Formally: ¬((Σ\{g})* · e · Σ*)
-- If g occurs first, e is unrestricted afterward.
noUntil :: Event t -> Event t -> RE t
noUntil e g = Not (Seq (Star (And (Single Wildcard) (Not (Single g)))) (Seq (Single e) top))

-- | @previously ev@ is the past-facing alias for 'finally'.
-- Use it as a /pre/-condition to assert that @ev@ occurred somewhere in
-- the preceding trace.  The underlying RE is @Σ* · ev · Σ*@, identical to
-- @finally ev@: the name exists purely to signal intent at the call site —
-- write @previously ev@ in @pre@ slots and @finally ev@ in @fut@ slots.
previously :: Event t -> RE t
previously = finally

-- ── Nullability: ν(r) ─────────────────────────────────────────────────────────
-- ν(r) = True  iff  ε ∈ L(r)

nullable :: RE t -> Bool
nullable Bot          = False
nullable Epsilon      = True
nullable (Single _)   = False
nullable (Seq r1 r2)  = nullable r1 && nullable r2
nullable (Or  r1 r2)  = nullable r1 || nullable r2
nullable (And r1 r2)  = nullable r1 && nullable r2
nullable (Star _)     = True
nullable (Not r)      = not (nullable r)   -- ν(¬r) = ¬ν(r)

-- ── Alphabet extraction ───────────────────────────────────────────────────────
-- Collect all concrete (non-Wildcard) events mentioned in an RE.
-- This forms the effective alphabet for complement unfolding in firstWith.

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

-- ── First Set ─────────────────────────────────────────────────────────────────
-- firstWith alph r: events in alph that can begin a word in L(r).
-- For Not r: e ∈ first(¬r)  iff  ∂_e(r) ≠ Σ*, i.e. some continuation after e
-- stays outside L(r).  We check this for every event in the supplied alphabet.

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

-- first r: convenience wrapper that uses the events in r itself as the alphabet.
-- subtraction passes the combined alphabet of both operands for completeness.
first :: Eq t => RE t -> [Event t]
first r = firstWith (atoms r) r

-- ── Brzozowski Derivative: ∂_e(r) ────────────────────────────────────────────
-- Key law for complement: ∂_a(¬r) = ¬(∂_a(r))

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

-- ── Antimirov Partial Derivatives: ∂_e^A(r) ──────────────────────────────────
-- antiDeriv e r returns a LIST of REs whose language UNION equals L(∂_e(r)).
-- This is the Antimirov set-based refinement of the Brzozowski derivative:
--   • Or  distributes into a union of smaller residuals.
--   • Seq factors out the tail r2, giving {t · r2 | t ∈ ∂_e^A(r1)} plus,
--     when r1 is nullable, the partial derivatives of r2 directly.
--   • Star unfolds one step: {t · r* | t ∈ ∂_e^A(r)}.
--   • And and Not have no canonical Antimirov splitting; they fall back to
--     the unique Brzozowski derivative wrapped in a singleton list.

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

-- Quotient r1 \ r2: the residual obligation in r2 after trace r1.
-- Base: if r1 = ε (nothing consumed), r2 is unchanged.
-- Σ* (Not Bot) trivially satisfies any precondition: residual is ⊤.
--
-- Rewritten using Antimirov partial derivatives on r2:
-- instead of a single Brzozowski step ∂_e(r2), we compute the full set
-- ∂_e^A(r2) of Antimirov residuals and recursively subtract ∂_e(r1) from
-- each element, then take their union with Or.  The result language is the
-- same as the Brzozowski version because ⋃ L(∂_e^A(r2)) = L(∂_e(r2)).
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

-- ── LTL ───────────────────────────────────────────────────────────────────────

data LTL t
    = LTLTrue
    | LTLFalse
    | LTLAtom     (Event t)
    | LTLNot      (LTL t)
    | LTLAnd      (LTL t) (LTL t)
    | LTLOr       (LTL t) (LTL t)
    | LTLNext     (LTL t)          -- X φ        (strong next)
    | LTLUntil    (LTL t) (LTL t)      -- φ U ψ
    | LTLFinally  (LTL t)          -- F φ  ≜  ⊤ U φ   ≡  Σ* · ⟦φ⟧
    | LTLGlobally (LTL t)          -- G φ  ≜  ¬F¬φ    ≡  ¬(Σ* · ¬⟦φ⟧)


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


-- Algebraic translation LTLf → RE (no automaton construction needed).
-- Complement is handled by the Not constructor directly.
-- LTLUntil returns Nothing when the left-hand side contains a temporal
-- operator with no single-step projection.
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

-- Normalization: simplify using RE algebra + De Morgan laws for Not.
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

-- ── Property-based tests ───────────────────────────────────────────────────────

-- Fixed small alphabet used for language-equality testing and RE generation.
testAlph :: [Event Term]
testAlph = [Atom "a" (List []), Atom "b" (List []), Atom "c" (List [])]

-- Language equality: check both REs on all words over testAlph up to length 3.
-- RE derives structural Eq, so (Seq (Seq a b) c) /= (Seq a (Seq b c))
-- even though they accept the same language; langEq gives the correct notion.
langEq :: RE Term -> RE Term -> Bool
langEq r1 r2 = all (\w -> run r1 w == run r2 w) testWords
  where
    testWords    = concatMap (`replicateM` testAlph) [0 .. 3]
    run r []     = nullable r
    run r (e:es) = run (normalize (derivative e r)) es

instance Arbitrary (RE Term) where
    arbitrary = sized genRE
      where
        genRE 0 = oneof [pure Bot, pure Epsilon, pure top, Single <$> elements testAlph]
        genRE n = oneof
            [ pure Bot
            , pure Epsilon
            , pure top
            , Single <$> elements testAlph
            , Seq  <$> genRE h <*> genRE h
            , Or   <$> genRE h <*> genRE h
            , And  <$> genRE h <*> genRE h
            , Star <$> genRE (n - 1)
            , Not  <$> genRE (n - 1)
            ]
          where h = n `div` 2
    shrink Bot         = []
    shrink Epsilon     = [Bot]
    shrink (Single _)  = [Bot, Epsilon]
    shrink (Not r)     = r : map Not  (shrink r)
    shrink (Star r)    = [Epsilon, r] ++ map Star (shrink r)
    shrink (Seq r1 r2) = [r1, r2] ++ [Seq r1' r2  | r1' <- shrink r1]
                                   ++ [Seq r1  r2' | r2' <- shrink r2]
    shrink (Or  r1 r2) = [r1, r2] ++ [Or  r1' r2  | r1' <- shrink r1]
                                   ++ [Or  r1  r2' | r2' <- shrink r2]
    shrink (And r1 r2) = [r1, r2] ++ [And r1' r2  | r1' <- shrink r1]
                                   ++ [And r1  r2' | r2' <- shrink r2]

-- (·) associativity:  (x · y) · z = x · (y · z)
prop_concat_assoc :: RE Term -> RE Term -> RE Term -> Property
prop_concat_assoc x y z =
    counterexample
        (  "LHS: " ++ show ((x · y) · z)
        ++ "\nRHS: " ++ show (x · (y · z))
        )
        (langEq ((x · y) · z) (x · (y · z)))

-- (·) left identity:  empty · x = x
prop_concat_left_id :: RE Term -> Bool
prop_concat_left_id x = langEq (empty · x) x

-- (·) right identity: x · empty = x
prop_concat_right_id :: RE Term -> Bool
prop_concat_right_id x = langEq (x · empty) x

-- (/\) associativity: (x /\ y) /\ z = x /\ (y /\ z)
prop_conj_assoc :: RE Term -> RE Term -> RE Term -> Bool
prop_conj_assoc x y z = langEq ((x /\ y) /\ z) (x /\ (y /\ z))

-- (/\) commutativity: x /\ y = y /\ x
prop_conj_comm :: RE Term -> RE Term -> Bool
prop_conj_comm x y = langEq (x /\ y) (y /\ x)

-- (/\) identity:      universe /\ x = x
prop_conj_id :: RE Term -> Bool
prop_conj_id x = langEq (universe /\ x) x

-- (\\) right zero:        x \\ empty = x
prop_sub_right_zero :: RE Term -> Bool
prop_sub_right_zero x = langEq (x \\ empty) x

-- (\\) universe residual: universe \\ x = universe
prop_sub_universe :: RE Term -> Property
prop_sub_universe x =
    let u :: RE Term = universe
    in
    counterexample
        (  "x:   " ++ show x
        ++ "\nLHS: " ++ show (u \\ x)
        ++ "\nRHS: " ++ show u
        )
        (langEq (u \\ x) u)

-- (\\) sequential dist.:  x \\ (a · b) = (x \\ b) \\ a
prop_sub_seq_dist :: RE Term -> RE Term -> RE Term -> Bool
prop_sub_seq_dist x a b = langEq (x \\ (a · b)) ((x \\ b) \\ a)

-- (\\) conjunction dist.: (a /\ b) \\ c = (a \\ c) /\ (b \\ c)
prop_sub_conj_dist :: RE Term -> RE Term -> RE Term -> Bool
prop_sub_conj_dist a b c = langEq ((a /\ b) \\ c) ((a \\ c) /\ (b \\ c))

runRELawTests :: IO ()
runRELawTests = do
    putStrLn "\n── RE Composable law tests (QuickCheck) ─────────────────────"
    putStrLn "-- (·) associativity"        >> quickCheck prop_concat_assoc
    putStrLn "-- (·) left identity"        >> quickCheck prop_concat_left_id
    putStrLn "-- (·) right identity"       >> quickCheck prop_concat_right_id
    putStrLn "-- (/\\) associativity"      >> quickCheck prop_conj_assoc
    putStrLn "-- (/\\) commutativity"      >> quickCheck prop_conj_comm
    putStrLn "-- (/\\) identity"           >> quickCheck prop_conj_id
    putStrLn "-- (\\\\) right zero"        >> quickCheck prop_sub_right_zero
    putStrLn "-- (\\\\) universe residual" >> quickCheck prop_sub_universe
    putStrLn "-- (\\\\) sequential dist."  >> quickCheck prop_sub_seq_dist
    putStrLn "-- (\\\\) conjunction dist." >> quickCheck prop_sub_conj_dist

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
