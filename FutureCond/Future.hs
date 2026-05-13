{-# LANGUAGE FunctionalDependencies #-}

module Future where

import Prelude hiding ((<>))
import Data.List (union, intercalate, subsequences, nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

-- ── Terms ─────────────────────────────────────────────────────────────────────

data Term = Var String | Str String | Num Int | List [Term]
    deriving (Eq)

instance Show Term where
    show (Var s) = s
    show (Str s) = "\"" ++ s ++ "\""
    show (Num n) = show n
    show (List ts) = "[" ++ intercalate ", " (map show ts) ++ "]"

-- ── Events ────────────────────────────────────────────────────────────────────

-- An Event is either a concrete named call or a wildcard pattern (Σ).
-- Wildcard is used only inside RE patterns (Single Wildcard ≡ Σ, any one step).
data Event = Atom String Term    -- e.g. send(x)
           | Wildcard            -- matches any single event
    deriving (Eq)

instance Show Event where
    show (Atom name arg) = name ++ "(" ++ show arg ++ ")"
    show Wildcard         = "_"

-- Does a concrete event occurrence match an event pattern in a Single?
subsumesEvent :: Event -> Event -> Bool
subsumesEvent _            Wildcard      = True   -- any occurrence matches wildcard pattern
subsumesEvent (Atom n1 a1) (Atom n2 a2) = n1 == n2 && a1 == a2
subsumesEvent Wildcard     (Atom _ _)   = False   -- wildcard occurrence ≠ specific pattern

-- ── Regular Expressions ───────────────────────────────────────────────────────

data RE
    = Bot          -- ∅         empty language
    | Epsilon      -- ε         empty word
    | Single Event -- a         single-event pattern
    | Seq RE RE    -- r₁ · r₂   concatenation
    | Or  RE RE    -- r₁ + r₂   union
    | And RE RE    -- r₁ ∩ r₂   intersection  (= ¬(¬r₁ + ¬r₂))
    | Star RE      -- r*        Kleene star
    | Not RE       -- ¬r        complement
    deriving (Eq)

instance Show RE where
    show Bot         = "∅"
    show Epsilon     = "ε"
    show (Single e)  = show e
    show (Seq r1 r2) = "(" ++ show r1 ++ ") · (" ++ show r2 ++ ")"
    show (Or  r1 r2) = "(" ++ show r1 ++ ") ∨ (" ++ show r2 ++ ")"
    show (And r1 r2) = "(" ++ show r1 ++ ") ∧ (" ++ show r2 ++ ")"
    show (Star r)    = "(" ++ show r ++ ")*"
    show (Not r)     = "¬(" ++ show r ++ ")"

-- Σ* — universal language, complement of the empty language
top :: RE
top = Not Bot

-- ── Nullability: ν(r) ─────────────────────────────────────────────────────────
-- ν(r) = True  iff  ε ∈ L(r)

nullable :: RE -> Bool
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

atoms :: RE -> [Event]
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

firstWith :: [Event] -> RE -> [Event]
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
first :: RE -> [Event]
first r = firstWith (atoms r) r

-- ── Brzozowski Derivative: ∂_e(r) ────────────────────────────────────────────
-- Key law for complement: ∂_a(¬r) = ¬(∂_a(r))

derivative :: Event -> RE -> RE
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

antiDeriv :: Event -> RE -> [RE]
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
reSubtraction :: RE -> RE -> RE
reSubtraction Epsilon r2 = r2
reSubtraction r1 r2 =
    let alph   = atoms r1 `union` atoms r2   -- combined alphabet for complement unfolding
        evts   = firstWith alph r1
        step e =
            let dr1  = normalize (derivative e r1)
                dr2s = antiDeriv e r2           -- Antimirov set of r2 residuals
            in foldr Or Bot (map (reSubtraction dr1) dr2s)
    in foldr Or Bot (map step evts)

-- ── LTL ───────────────────────────────────────────────────────────────────────

data LTL
    = LTLTrue
    | LTLFalse
    | LTLAtom     Event
    | LTLNot      LTL
    | LTLAnd      LTL LTL
    | LTLOr       LTL LTL
    | LTLNext     LTL          -- X φ        (strong next)
    | LTLUntil    LTL LTL      -- φ U ψ
    | LTLFinally  LTL          -- F φ  ≜  ⊤ U φ   ≡  Σ* · ⟦φ⟧
    | LTLGlobally LTL          -- G φ  ≜  ¬F¬φ    ≡  ¬(Σ* · ¬⟦φ⟧)
    deriving (Eq, Show)

-- ── Single-step projection ────────────────────────────────────────────────────
-- toSingleStep l: the RE for a single event satisfying l at the current step.
-- This is the correct building block for LTLUntil:
--   ⟦φ U ψ⟧  =  toSingleStep(φ)* · ⟦ψ⟧
--
-- Using ltl_to_re l1 directly would be wrong: ⟦l1⟧ may contain words of
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
toSingleStep :: LTL -> Maybe RE
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
ltl_to_re :: LTL -> Maybe RE
ltl_to_re LTLTrue            = Just top                         -- ¬∅  = Σ*
ltl_to_re LTLFalse           = Just Bot                              -- ∅
ltl_to_re (LTLAtom e)        = Just (Single e)
ltl_to_re (LTLNot l)         = Not <$> ltl_to_re l                  -- ¬⟦l⟧
ltl_to_re (LTLAnd l1 l2)     = And <$> ltl_to_re l1 <*> ltl_to_re l2  -- ⟦l1⟧ ∩ ⟦l2⟧
ltl_to_re (LTLOr  l1 l2)     = Or  <$> ltl_to_re l1 <*> ltl_to_re l2  -- ⟦l1⟧ ∪ ⟦l2⟧
ltl_to_re (LTLNext l)        = Seq (Single Wildcard) <$> ltl_to_re l   -- Σ · ⟦l⟧
ltl_to_re (LTLUntil l1 l2)   = Seq . Star <$> toSingleStep l1          -- step(l1)* · ⟦l2⟧
                                           <*> ltl_to_re l2
ltl_to_re (LTLFinally l)     = Seq top <$> ltl_to_re l            -- Σ* · ⟦l⟧
ltl_to_re (LTLGlobally l)    = Not . Seq top . Not                 -- ¬(Σ* · ¬⟦l⟧)
                                   <$> ltl_to_re l

-- ── Shorthands ────────────────────────────────────────────────────────────────

-- G ev  — ev must occur at every step:  ev*
globally :: Event -> RE
globally ev = Star (Single ev)

-- F ev  — ev must occur at some step:  Σ* · ev · Σ*
finally :: Event -> RE
finally ev = Seq top (Seq (Single ev) top)

-- ── Composable class ──────────────────────────────────────────────────────────

class Composable a where
    concatenation :: a -> a -> a
    conjunction   :: a -> a -> a
    empty         :: a
    universe      :: a
    subtraction   :: a -> a -> a

infixl 6 <>
(<>) :: Composable a => a -> a -> a
(<>) = concatenation

infixl 7 /\
(/\) :: Composable a => a -> a -> a
(/\) = conjunction

infixl 5 \\
(\\) :: Composable a => a -> a -> a
(\\) = subtraction

instance Composable RE where
    concatenation = Seq
    conjunction   = And
    empty         = Epsilon
    universe      = top
    subtraction   = reSubtraction

-- Normalization: simplify using RE algebra + De Morgan laws for Not.
normalize :: RE -> RE
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

-- ── Effectful monad ───────────────────────────────────────────────────────────

data Effectful eff a = Effectful
    { ret    :: a
    , pre    :: eff
    , post   :: eff
    , future :: eff
    }

instance Functor (Effectful eff) where
    fmap f e = e { ret = f (ret e) }

instance Composable eff => Applicative (Effectful eff) where
    pure x = Effectful
        { ret    = x
        , pre    = universe
        , post   = empty
        , future = universe
        }
    ef <*> ex = Effectful
        { ret    = ret ef (ret ex)
        -- 1. ef is evaluated first (left to right)
        -- 2. ex is evaluated second
        -- 3. the extracted function is applied to the extracted value
        -- Traditional precondition: pre of ef, plus whatever of pre ex
        -- is not already discharged by post ef.
        -- If post ef ⊢ pre ex, the quotient is ε and nothing is added.
        , pre    = pre ef /\ (post ef \\ pre ex)
        , post   = post ef <> post ex
        , future = (post ex \\ future ef) /\ future ex
        }

instance Composable eff => Monad (Effectful eff) where
    return = pure
    e >>= f = let fe = f (ret e) in Effectful
        { ret    = ret fe
        -- Traditional precondition: pre of e, plus the residual of pre fe
        -- not covered by post e.  Mirrors the Hoare rule:
        --   {P} e {Q},  Q ⊢ P'  ⊢  {P} e >>= f {R}
        -- When post e fully satisfies pre fe, pre fe \\ post e = ε.
        , pre    = pre e /\ (post e \\ pre fe)
        , post   = post e <> post fe
        , future = (post fe \\ future e) /\ future fe
        }


-- ── Separation Logic ──────────────────────────────────────────────────────────
-- Symbolic separation-logic predicates over integer-addressed heaps.
-- Parallels the RE instance: Star/Emp play the role of Seq/Epsilon,
-- Conj/Top play the role of And/(Not Bot), and Wand plays the role of
-- the Brzozowski quotient.

type Addr = Int
type Val  = Int
type Heap = Map.Map Addr Val

data SL
    = Emp              -- empty heap                  (identity for Sep)
    | Top              -- any heap (universe)          (identity for Conj)
    | Cell Addr Val    -- singleton: address l holds value v
    | SepStar SL SL       -- P * Q   separating conjunction
    | Conj SL SL       -- P ∧ Q   ordinary conjunction
    | Wand SL SL       -- P -* Q  magic wand (residual)
    deriving (Eq, Show)

-- ── Semantic interpretation ───────────────────────────────────────────────────
-- satisfies world heap predicate
-- The Wand case quantifies over sub-heaps of the supplied world, bounding
-- the search to a finite domain for decidable checking.

satisfies :: Heap -> Heap -> SL -> Bool
satisfies _ h Emp        = Map.null h
satisfies _ _ Top        = True
satisfies _ h (Cell l v) = h == Map.singleton l v
satisfies w h (SepStar p q) = any split (subHeaps h)
  where
    split h1 = let h2 = Map.difference h h1
               in  heapDisjoint h1 h2
                && satisfies w h1 p
                && satisfies w h2 q
satisfies w h (Conj p q) = satisfies w h p && satisfies w h q
satisfies w h (Wand p q) = all step (subHeaps w)
  where
    step h' = not (heapDisjoint h h' && satisfies w h' p)
           || satisfies w (Map.union h h') q

-- All sub-heaps of a heap (powerset of key-value pairs).
subHeaps :: Heap -> [Heap]
subHeaps = map Map.fromList . subsequences . Map.toList

heapDisjoint :: Heap -> Heap -> Bool
heapDisjoint h1 h2 = Set.disjoint (Map.keysSet h1) (Map.keysSet h2)

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
