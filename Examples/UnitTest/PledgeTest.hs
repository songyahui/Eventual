module Examples.UnitTest.PledgeTest where

import Prelude hiding ((<>))
import Data.IORef
import Data.List (sort)
import Data.Maybe (isNothing)
import Pledge

-- ── Helpers ───────────────────────────────────────────────────────────────────

-- Convenience events used throughout
a, b, c :: Event Term
a = Atom "a" (List [])
b = Atom "b" (List [])
c = Atom "c" (List [])

-- Report pass/fail; increment counter; crash on first failure so the culprit is visible.
check :: IORef Int -> String -> Bool -> IO ()
check counter name result = do
    modifyIORef' counter (+1)
    case result of
        True  -> putStrLn $ "  PASS  " ++ name
        False -> error    $ "\n  FAIL  " ++ name

-- Order-independent event-list equality (show-based, to avoid needing Ord).
sameSet :: [Event Term] -> [Event Term] -> Bool
sameSet xs ys = sort (map show xs) == sort (map show ys)

-- Fold derivatives over a word, normalising at each step, then check
-- nullability.  This is the standard Brzozowski membership test and a
-- useful illustration of how derivative composes.
matches :: RE Term -> [Event Term] -> Bool
matches r []     = nullable r
matches r (e:es) = matches (normalize (derivative e r)) es

-- ── subsumesEvent ─────────────────────────────────────────────────────────────
-- subsumesEvent e p: does occurrence e match pattern p?
-- Wildcard as pattern accepts everything; Wildcard as occurrence matches nothing specific.

test_subsumesEvent :: IORef Int -> IO ()
test_subsumesEvent counter = do
    putStrLn "\n── subsumesEvent ────────────────────────────────────────────────"

    let x = Str "x"
        sendX = Atom "send" x
        recvX = Atom "recv" x
        sendY = Atom "send" (Str "y")
        send1 = Atom "send" (Num 1)
        sendL = Atom "send" (List [Num 1, Num 2])

    -- Wildcard pattern subsumes any occurrence
    check counter "subsumesEvent a        Wildcard = True   (concrete event matches wildcard)" $
        subsumesEvent a Wildcard

    check counter "subsumesEvent Wildcard Wildcard = True   (wildcard occurrence matches wildcard pattern)" $
        subsumesEvent (Wildcard :: Event Term) Wildcard

    check counter "subsumesEvent send(x)  Wildcard = True   (any atom matches wildcard)" $
        subsumesEvent sendX Wildcard

    -- Wildcard occurrence vs concrete pattern
    check counter "subsumesEvent Wildcard send(x) = False  (wildcard occ ≠ specific pattern)" $
        not (subsumesEvent Wildcard sendX)

    -- Identical atoms
    check counter "subsumesEvent send(x) send(x) = True    (same name, same arg)" $
        subsumesEvent sendX sendX

    check counter "subsumesEvent send(1) send(1) = True    (same name, Num arg)" $
        subsumesEvent send1 send1

    -- Different names
    check counter "subsumesEvent send(x) recv(x) = False   (different name)" $
        not (subsumesEvent sendX recvX)

    -- Same name, different args
    check counter "subsumesEvent send(x) send(y) = False   (same name, different Str arg)" $
        not (subsumesEvent sendX sendY)

    check counter "subsumesEvent send(x) send(1) = False   (Str vs Num arg)" $
        not (subsumesEvent sendX send1)

    -- List term
    check counter "subsumesEvent send([1,2]) send([1,2]) = True    (List arg equal)" $
        subsumesEvent sendL sendL

    check counter "subsumesEvent send([1,2]) send(x) = False   (List vs Str)" $
        not (subsumesEvent sendL sendX)

    check counter "subsumesEvent send([1,2]) Wildcard = True   (List atom matches wildcard)" $
        subsumesEvent sendL Wildcard

-- ── nullable ──────────────────────────────────────────────────────────────────
-- ν(r) = True  iff  ε ∈ L(r).

test_nullable :: IORef Int -> IO ()
test_nullable counter = do
    putStrLn "\n── nullable ─────────────────────────────────────────────────────"

    -- Base cases
    check counter "nullable Bot     = False   (empty language contains no word at all)"  $
        not (nullable Bot)

    check counter "nullable Epsilon = True    (ε is the only word in {ε})"  $
        nullable Epsilon

    check counter "nullable (Single a) = False  (a requires exactly one event)"  $
        not (nullable (Single a))

    check counter "nullable (Single _) = False  (wildcard still consumes one step)"  $
        not (nullable (Single Wildcard))

    -- Sequence: ε ∈ r1·r2  iff  ε ∈ r1  AND  ε ∈ r2
    check counter "nullable (ε · ε) = True"  $
        nullable (Seq Epsilon Epsilon)

    check counter "nullable (a · b) = False"  $
        not (nullable (Seq (Single a) (Single b)))

    check counter "nullable (ε · a) = False  (tail is not nullable)"  $
        not (nullable (Seq Epsilon (Single a)))

    -- Union: ε ∈ r1 + r2  iff  ε ∈ r1  OR  ε ∈ r2
    check counter "nullable (∅ ∨ ε) = True"  $
        nullable (Or Bot Epsilon)

    check counter "nullable (a ∨ b) = False"  $
        not (nullable (Or (Single a) (Single b)))

    -- Intersection: ε ∈ r1 ∧ r2  iff  ε ∈ r1  AND  ε ∈ r2
    check counter "nullable (ε ∧ ε) = True"  $
        nullable (And Epsilon Epsilon)

    check counter "nullable (a ∧ ε) = False"  $
        not (nullable (And (Single a) Epsilon))

    -- Star: zero repetitions always accepted
    check counter "nullable (a*) = True   (zero copies of a)"  $
        nullable (Star (Single a))

    check counter "nullable (∅*) = True   (∅* = ε by RE algebra)"  $
        nullable (Star Bot)

    -- Complement: ν(¬r) = ¬ν(r)
    check counter "nullable (¬ε) = False  (ε ∉ complement of {ε})"  $
        not (nullable (Not Epsilon))

    check counter "nullable (¬∅) = True   (¬∅ = Σ*, which contains ε)"  $
        nullable (Not Bot)

    check counter "nullable (¬a) = True   (ε ∉ {a}, so ε ∈ ¬{a})"  $
        nullable (Not (Single a))

-- ── derivative ────────────────────────────────────────────────────────────────
-- ∂_e(r): residual RE recognised by continuations after consuming event e.
-- Key law for complement: ∂_e(¬r) = ¬(∂_e(r)).

test_derivative :: IORef Int -> IO ()
test_derivative counter = do
    putStrLn "\n── derivative ───────────────────────────────────────────────────"

    -- Trivial languages
    check counter "∂_a(∅) = ∅"  $
        derivative a Bot == Bot

    check counter "∂_a(ε) = ∅   (ε accepts nothing after consuming a)"  $
        derivative a Epsilon == Bot

    -- Single-event patterns
    check counter "∂_a(a) = ε   (consuming the sole event leaves empty continuation)"  $
        derivative a (Single a) == Epsilon

    check counter "∂_a(b) = ∅   (a ≠ b, pattern unmatched)"  $
        derivative a (Single b) == Bot

    check counter "∂_a(_) = ε   (wildcard matches any single event)"  $
        derivative a (Single Wildcard) == Epsilon

    -- Sequence, non-nullable head: ∂_e(r1 · r2) = ∂_e(r1) · r2
    check counter "∂_a(a · b) = ε · b   (head consumed, tail remains)"  $
        derivative a (Seq (Single a) (Single b)) == Seq Epsilon (Single b)

    check counter "∂_a(b · a) = ∅ · a   (a cannot start b·a)"  $
        derivative a (Seq (Single b) (Single a)) == Seq Bot (Single a)

    -- Sequence, nullable head: Brzozowski split
    -- ∂_e(ε · r2) = (∂_e(ε) · r2) ∨ ∂_e(r2) = (∅ · r2) ∨ ∂_e(r2)
    check counter "∂_a(ε · a) = (∅ · a) ∨ ε   (nullable head triggers split)"  $
        derivative a (Seq Epsilon (Single a))
            == Or (Seq Bot (Single a)) Epsilon

    -- Union: derivative distributes
    check counter "∂_a(a ∨ b) = ε ∨ ∅"  $
        derivative a (Or (Single a) (Single b)) == Or Epsilon Bot

    check counter "∂_b(a ∨ b) = ∅ ∨ ε"  $
        derivative b (Or (Single a) (Single b)) == Or Bot Epsilon

    -- Intersection: derivative distributes
    check counter "∂_a(a ∧ a) = ε ∧ ε"  $
        derivative a (And (Single a) (Single a)) == And Epsilon Epsilon

    check counter "∂_a(a ∧ b) = ε ∧ ∅"  $
        derivative a (And (Single a) (Single b)) == And Epsilon Bot

    -- Star: ∂_e(r*) = ∂_e(r) · r*
    check counter "∂_a(a*) = ε · a*"  $
        derivative a (Star (Single a)) == Seq Epsilon (Star (Single a))

    check counter "∂_a(b*) = ∅ · b*   (a cannot start b*)"  $
        derivative a (Star (Single b)) == Seq Bot (Star (Single b))

    -- Complement: ∂_e(¬r) = ¬(∂_e(r))
    check counter "∂_a(¬a) = ¬ε   (complement distributes over derivative)"  $
        derivative a (Not (Single a)) == Not Epsilon

    check counter "∂_a(¬b) = ¬∅   (a ≠ b, so derivative of b is ∅; complement gives Σ*)"  $
        derivative a (Not (Single b)) == Not Bot

    -- Membership via iterated derivative + nullability
    -- [a]     ∈ L(a)       ↔  nullable(∂_a(a))        = nullable(ε) = True
    check counter "word [a] matches L(a)"  $
        matches (Single a) [a]

    -- [a,b]   ∈ L(a · b)
    check counter "word [a,b] matches L(a · b)"  $
        matches (Seq (Single a) (Single b)) [a, b]

    -- [a]     ∉ L(a · b)   (too short)
    check counter "word [a] does not match L(a · b)"  $
        not (matches (Seq (Single a) (Single b)) [a])

    -- [a,a,a] ∈ L(a*)
    check counter "word [a,a,a] matches L(a*)"  $
        matches (Star (Single a)) [a, a, a]

    -- derivative of finally(free(1)) after free(1) normalises to Σ* (universe)
    -- (this is the original test_derivative example, preserved)
    check counter "∂_free(1)(finally(free(1))) normalises to Σ*"  $
        let r = finally (Atom "free" (List [Num 1]))
            e = Atom "free" (List [Num 1])
        in normalize (derivative e r) == universe

-- ── atoms ─────────────────────────────────────────────────────────────────────
-- Collect all concrete (non-Wildcard) events mentioned in an RE.
-- This alphabet drives complement unfolding inside firstWith.

test_atoms :: IORef Int -> IO ()
test_atoms counter = do
    putStrLn "\n── atoms ────────────────────────────────────────────────────────"

    check counter "atoms ∅ = []"  $
        null (atoms (Bot :: RE Term))

    check counter "atoms ε = []"  $
        null (atoms (Epsilon :: RE Term))

    check counter "atoms (_) = []   (Wildcard contributes no concrete event)"  $
        null (atoms (Single Wildcard :: RE Term))

    check counter "atoms (a) = [a]"  $
        atoms (Single a) == [a]

    check counter "atoms (a ∨ b) = {a, b}"  $
        sameSet (atoms (Or  (Single a) (Single b))) [a, b]

    check counter "atoms (a ∧ b) = {a, b}"  $
        sameSet (atoms (And (Single a) (Single b))) [a, b]

    check counter "atoms (a · a) = [a]   (union deduplicates repeated events)"  $
        atoms (Seq (Single a) (Single a)) == [a]

    check counter "atoms (a · b) = {a, b}"  $
        sameSet (atoms (Seq (Single a) (Single b))) [a, b]

    check counter "atoms (a*) = [a]"  $
        atoms (Star (Single a)) == [a]

    check counter "atoms (¬a) = [a]   (complement preserves the alphabet)"  $
        atoms (Not (Single a)) == [a]

    check counter "atoms (a ∧ b ∧ c) = {a, b, c}"  $
        sameSet (atoms (And (Single a) (And (Single b) (Single c)))) [a, b, c]

    check counter "atoms (a · _ · b) = {a, b}   (wildcard not counted)"  $
        sameSet (atoms (Seq (Single a) (Seq (Single Wildcard) (Single b)))) [a, b]

-- ── first / firstWith ─────────────────────────────────────────────────────────
-- first r       : events that can begin a word in L(r); alphabet = atoms r.
-- firstWith α r : same but using an explicitly supplied alphabet α.
-- For Not r, an event e is in the first set iff ∂_e(r) ≠ Σ* (isTotal check).

test_first :: IORef Int -> IO ()
test_first counter = do
    putStrLn "\n── first / firstWith ────────────────────────────────────────────"

    check counter "first ∅ = []"  $
        null (first (Bot :: RE Term))

    check counter "first ε = []   (ε starts with no event)"  $
        null (first (Epsilon :: RE Term))

    check counter "first (a) = [a]"  $
        first (Single a) == [a]

    -- Wildcard: firstWith uses the Wildcard as-is when it appears as Single
    check counter "first (_) = [_]   (wildcard event returned; alphabet from atoms is empty)"  $
        first (Single Wildcard :: RE Term) == [Wildcard]

    -- Sequence: non-nullable head — only head's first set matters
    check counter "first (a · b) = [a]   (b unreachable as first event)"  $
        first (Seq (Single a) (Single b)) == [a]

    -- Sequence: nullable head — both head and tail contribute
    check counter "first (ε · b) = [b]   (ε nullable, so tail's first set is also included)"  $
        sameSet (first (Seq Epsilon (Single b))) [b]

    check counter "first (a* · b) = {a, b}   (a* nullable, so b is also reachable as first)"  $
        sameSet (first (Seq (Star (Single a)) (Single b))) [a, b]

    -- Union: union of both first sets
    check counter "first (a ∨ b) = {a, b}"  $
        sameSet (first (Or (Single a) (Single b))) [a, b]

    check counter "first (a ∨ a) = [a]   (no duplicates from union)"  $
        first (Or (Single a) (Single a)) == [a]

    -- Intersection: only events present in BOTH first sets
    check counter "first (a ∧ a) = [a]"  $
        first (And (Single a) (Single a)) == [a]

    check counter "first (a ∧ b) = []   (a and b share no first event)"  $
        null (first (And (Single a) (Single b)))

    check counter "first ((a ∨ b) ∧ (b ∨ c)) = [b]   (only b is in both first sets)"  $
        first (And (Or (Single a) (Single b))
                   (Or (Single b) (Single c))) == [b]

    -- Star
    check counter "first (a*) = [a]"  $
        first (Star (Single a)) == [a]

    check counter "first (∅*) = []   (∅* = ε, no events)"  $
        null (first (Star Bot :: RE Term))

    -- firstWith: complement unfolding with an explicit alphabet
    --
    -- firstWith [a,b] (¬a):
    --   e=a: ∂_a(a) = ε, not Σ* → a included  (e.g. a·a ∈ ¬{a})
    --   e=b: ∂_b(a) = ∅, not Σ* → b included  (e.g. b   ∈ ¬{a})
    check counter "firstWith {a,b} (¬a) = {a,b}   (both can start words in ¬{a})"  $
        sameSet (firstWith [a, b] (Not (Single a))) [a, b]

    -- firstWith [a] (¬∅) = [a]:
    --   ∂_a(∅) = ∅, not Σ* → a included  (any event can start Σ* = ¬∅)
    check counter "firstWith {a} (¬∅) = [a]   (Σ* starts with every alphabet event)"  $
        firstWith [a] (Not Bot) == [a]

    -- firstWith [a,b] (¬Σ*) = []:
    --   Not Bot is Σ*.  ∂_a(Bot) = Bot; isTotal Bot = False; not False = True...
    --   Wait — ¬Σ* = Bot.  We test Not (Not Bot).
    --   ∂_e(Not Bot) = Not (∂_e Bot) = Not Bot (= Σ*); isTotal (Not Bot) = True → excluded.
    check counter "firstWith {a,b} (¬Σ*) = []   (¬Σ* = ∅, no first events)"  $
        null (firstWith [a, b] (Not (Not Bot)))

    -- first (¬∅) = [] when atoms is empty:
    -- atoms (Not Bot) = atoms Bot = []; so firstWith [] (Not Bot) = [].
    check counter "first (¬∅) = []   (no concrete atoms in RE → empty alphabet for unfolding)"  $
        null (first (Not Bot :: RE Term))

    -- firstWith with Wildcard-bearing RE: atoms does not include Wildcard,
    -- but if we supply the alphabet manually we get the right answer.
    -- firstWith {a,b} (¬_):
    --   ∂_e(Single Wildcard) = Epsilon (wildcard matches any event).
    --   isTotal Epsilon = False, so every e in the alphabet is included.
    --   ¬_ accepts words of length ≠ 1; both a and b can begin such words
    --   (e.g. a·a ∈ ¬{_} has length 2).
    check counter "firstWith {a,b} (¬_) = {a,b}   (both events can start length-≠1 words)"  $
        sameSet (firstWith [a, b] (Not (Single Wildcard))) [a, b]

    -- ── Not-specific firstWith cases ──────────────────────────────────────────

    -- Double negation: ¬¬a = a, so only a can start words in L(a).
    -- ∂_a(¬a) = ¬ε  → not Σ* → a included
    -- ∂_b(¬a) = ¬∅ = Σ* → isTotal → b excluded
    check counter "firstWith {a,b} (¬¬a) = [a]   (double negation restores original first set)"  $
        firstWith [a, b] (Not (Not (Single a))) == [a]

    -- Not of Seq: ¬(a·b) excludes only the word [a,b]; every event in {a,b,c}
    -- can begin some word in the complement (e.g. a·a, b, c ∈ ¬(a·b)).
    -- ∂_a(a·b) = b  (not Σ*) → a included
    -- ∂_b(a·b) = ∅  (not Σ*) → b included
    -- ∂_c(a·b) = ∅  (not Σ*) → c included
    check counter "firstWith {a,b,c} (¬(a·b)) = {a,b,c}   (all events can start words in complement of seq)"  $
        sameSet (firstWith [a, b, c] (Not (Seq (Single a) (Single b)))) [a, b, c]

    -- Not of Star: ¬(a*) accepts any word with at least one non-a event.
    -- ∂_a(a*) = a* (not Σ*) → a included  (e.g. a·b ∈ ¬(a*))
    -- ∂_b(a*) = ∅  (not Σ*) → b included  (e.g. b   ∈ ¬(a*))
    check counter "firstWith {a,b} (¬(a*)) = {a,b}   (both events can start words outside a*)"  $
        sameSet (firstWith [a, b] (Not (Star (Single a)))) [a, b]

    -- Not of Or: ¬(a ∨ b) excludes single-event words [a] and [b]; multi-event
    -- words and [c] remain.  All three events can open such continuations.
    -- ∂_a(a ∨ b) = ε (not Σ*) → a included  (e.g. a·a ∈ ¬(a∨b))
    -- ∂_b(a ∨ b) = ε (not Σ*) → b included  (e.g. b·b ∈ ¬(a∨b))
    -- ∂_c(a ∨ b) = ∅ (not Σ*) → c included  (e.g. c   ∈ ¬(a∨b))
    check counter "firstWith {a,b,c} (¬(a ∨ b)) = {a,b,c}   (complement of union)"  $
        sameSet (firstWith [a, b, c] (Not (Or (Single a) (Single b)))) [a, b, c]

    -- Not of And with disjoint languages: L(a) ∩ L(b) = ∅, so ¬(a ∧ b) = ¬∅ = Σ*.
    -- Every event starts some word in Σ*.
    -- ∂_a(a ∧ b) = ε ∧ ∅ = ∅ (not Σ*) → a included
    -- ∂_b(a ∧ b) = ∅ ∧ ε = ∅ (not Σ*) → b included
    check counter "firstWith {a,b} (¬(a ∧ b)) = {a,b}   (¬∅ = Σ*, every event is a first event)"  $
        sameSet (firstWith [a, b] (Not (And (Single a) (Single b)))) [a, b]

    -- Alphabet strictly smaller than the atoms of the inner RE:
    -- only events in the supplied alphabet are candidates.
    -- ∂_a(b) = ∅ (not Σ*) → a included; b is not in the alphabet so never checked.
    check counter "firstWith {a} (¬b) = [a]   (event outside alphabet is not a candidate)"  $
        firstWith [a] (Not (Single b)) == [a]

    -- Not of (a · Σ*): ¬(a·Σ*) = words that do NOT start with a.
    -- ∂_a(a·Σ*) = Σ* → isTotal → a excluded
    -- ∂_b(a·Σ*) = ∅ (not Σ*) → b included  (e.g. b ∈ ¬(a·Σ*))
    check counter "firstWith {a,b} (¬(a·Σ*)) = [b]   (only b can start words not beginning with a)"  $
        firstWith [a, b] (Not (Seq (Single a) (Not Bot))) == [b]

    -- Not of (a·Σ* ∨ b·Σ*): excludes all words starting with a or b; only c remains.
    -- ∂_a(...) = Σ* → a excluded
    -- ∂_b(...) = Σ* → b excluded
    -- ∂_c(...) = ∅  (not Σ*) → c included  (e.g. c ∈ ¬(a·Σ* ∨ b·Σ*))
    check counter "firstWith {a,b,c} (¬(a·Σ* ∨ b·Σ*)) = [c]   (only c avoids both excluded prefixes)"  $
        firstWith [a, b, c]
            (Not (Or (Seq (Single a) (Not Bot))
                     (Seq (Single b) (Not Bot)))) == [c]

    -- De Morgan unfolding: ¬(¬a ∧ ¬b) = ¬¬(a ∨ b) = a ∨ b.
    -- ∂_a(¬a ∧ ¬b) = ¬ε ∧ Σ* = ¬ε (not Σ*) → a included
    -- ∂_b(¬a ∧ ¬b) = Σ* ∧ ¬ε = ¬ε (not Σ*) → b included
    -- ∂_c(¬a ∧ ¬b) = Σ* ∧ Σ* = Σ* → isTotal → c excluded
    check counter "firstWith {a,b,c} (¬(¬a ∧ ¬b)) = {a,b}   (De Morgan: equivalent to first (a ∨ b))"  $
        sameSet (firstWith [a, b, c] (Not (And (Not (Single a)) (Not (Single b))))) [a, b]

-- ── normalize ─────────────────────────────────────────────────────────────────
-- Simplify an RE using RE algebra + De Morgan laws.
-- Base cases (Bot, Epsilon, Single) are returned unchanged.

test_normalize :: IORef Int -> IO ()
test_normalize counter = do
    putStrLn "\n── normalize ────────────────────────────────────────────────────"

    -- Base cases: atoms are already normal
    check counter "normalize ∅ = ∅" $
        normalize (Bot :: RE Term) == Bot

    check counter "normalize ε = ε" $
        normalize (Epsilon :: RE Term) == Epsilon

    check counter "normalize a = a" $
        normalize (Single a) == Single a

    -- ── Seq ───────────────────────────────────────────────────────────────────

    check counter "normalize (∅ · a) = ∅   (Bot left absorbs)" $
        normalize (Seq Bot (Single a)) == Bot

    check counter "normalize (a · ∅) = ∅   (Bot right absorbs)" $
        normalize (Seq (Single a) Bot) == Bot

    check counter "normalize (ε · a) = a   (Epsilon left identity)" $
        normalize (Seq Epsilon (Single a)) == Single a

    check counter "normalize (a · ε) = a   (Epsilon right identity)" $
        normalize (Seq (Single a) Epsilon) == Single a

    check counter "normalize (a · b) = a · b   (no simplification)" $
        normalize (Seq (Single a) (Single b)) == Seq (Single a) (Single b)

    -- nested: inner Bot propagates outward
    check counter "normalize ((∅ · a) · b) = ∅   (nested Bot collapses)" $
        normalize (Seq (Seq Bot (Single a)) (Single b)) == Bot

    -- ── Or ────────────────────────────────────────────────────────────────────

    check counter "normalize (∅ ∨ a) = a   (Bot left identity for Or)" $
        normalize (Or Bot (Single a)) == Single a

    check counter "normalize (a ∨ ∅) = a   (Bot right identity for Or)" $
        normalize (Or (Single a) Bot) == Single a

    check counter "normalize (a ∨ a) = a   (idempotent)" $
        normalize (Or (Single a) (Single a)) == Single a

    check counter "normalize (a ∨ Σ*) = Σ*   (top absorbs on right)" $
        normalize (Or (Single a) (Not Bot)) == Not Bot

    check counter "normalize (Σ* ∨ a) = Σ*   (top absorbs on left)" $
        normalize (Or (Not Bot) (Single a)) == Not Bot

    check counter "normalize (a ∨ b) = a ∨ b   (no simplification)" $
        normalize (Or (Single a) (Single b)) == Or (Single a) (Single b)

    -- ── And ───────────────────────────────────────────────────────────────────

    check counter "normalize (∅ ∧ a) = ∅   (Bot left zero for And)" $
        normalize (And Bot (Single a)) == Bot

    check counter "normalize (a ∧ ∅) = ∅   (Bot right zero for And)" $
        normalize (And (Single a) Bot) == Bot

    check counter "normalize (a ∧ a) = a   (idempotent)" $
        normalize (And (Single a) (Single a)) == Single a

    check counter "normalize (Σ* ∧ a) = a   (top left identity for And)" $
        normalize (And (Not Bot) (Single a)) == Single a

    check counter "normalize (a ∧ Σ*) = a   (top right identity for And)" $
        normalize (And (Single a) (Not Bot)) == Single a

    -- ε ∧ r: keep ε only if r is nullable
    check counter "normalize (ε ∧ a*) = ε   (ε ∧ nullable = ε)" $
        normalize (And Epsilon (Star (Single a))) == Epsilon

    check counter "normalize (ε ∧ a) = ∅   (ε ∧ non-nullable = ∅)" $
        normalize (And Epsilon (Single a)) == Bot

    check counter "normalize (a ∧ ε) = ∅   (non-nullable ∧ ε = ∅)" $
        normalize (And (Single a) Epsilon) == Bot

    check counter "normalize (a ∧ b) = a ∧ b   (no simplification)" $
        normalize (And (Single a) (Single b)) == And (Single a) (Single b)

    -- ── Not ───────────────────────────────────────────────────────────────────

    check counter "normalize (¬¬a) = a   (double negation)" $
        normalize (Not (Not (Single a))) == Single a

    check counter "normalize (¬∅) = Σ*   (complement of empty = top)" $
        normalize (Not Bot :: RE Term) == Not Bot

    check counter "normalize (¬Σ*) = ∅   (complement of top = empty)" $
        normalize (Not (Not Bot) :: RE Term) == Bot

    -- De Morgan: ¬(r1 ∨ r2) = ¬r1 ∧ ¬r2
    check counter "normalize (¬(a ∨ b)) = ¬a ∧ ¬b   (De Morgan)" $
        normalize (Not (Or (Single a) (Single b)))
            == And (Not (Single a)) (Not (Single b))

    -- De Morgan: ¬(r1 ∧ r2) = ¬r1 ∨ ¬r2
    check counter "normalize (¬(a ∧ b)) = ¬a ∨ ¬b   (De Morgan)" $
        normalize (Not (And (Single a) (Single b)))
            == Or (Not (Single a)) (Not (Single b))

    -- involution chains
    check counter "normalize (¬¬¬a) = ¬a   (triple negation)" $
        normalize (Not (Not (Not (Single a)))) == Not (Single a)

    -- ── Star ──────────────────────────────────────────────────────────────────

    check counter "normalize (∅*) = ε   (empty Kleene = epsilon)" $
        normalize (Star Bot :: RE Term) == Epsilon

    check counter "normalize (ε*) = ε   (epsilon Kleene = epsilon)" $
        normalize (Star Epsilon :: RE Term) == Epsilon

    check counter "normalize (a*) = a*   (no simplification)" $
        normalize (Star (Single a)) == Star (Single a)

    -- inner simplification: (∅ · a)* = ∅* = ε
    check counter "normalize ((∅ · a)*) = ε   (inner Bot collapses before Star)" $
        normalize (Star (Seq Bot (Single a))) == Epsilon

-- ── reLeftQuotient ─────────────────────────────────────────────────────────────
-- r1 \\ r2: residual obligation in r2 after trace r1.
-- Implemented via Antimirov partial derivatives on r2: instead of a single
-- Brzozowski step, antiDeriv yields a LIST of residuals whose language union
-- equals L(∂_e(r2)); we subtract the remaining r1 from each and join with Or.
--
-- Key semantic contract:
--   ε \\ r2 = r2            (base case: nothing consumed)
--   a \\ a  = ε             (exact match: obligation discharged, ε remains)
--   a \\ (a·b) = b          (prefix: tail obligation remains)
--   (a·b) \\ a = ∅          (overshoot: no valid continuation)
-- Antimirov-specific behaviour:
--   a \\ (a ∨ (a·b)) = ε ∨ b   (both Or-branches fire as separate residuals)
--   b \\ (a*·b) = ε             (nullable head a* skipped via Antimirov nullable split)

test_reLeftQuotient :: IORef Int -> IO ()
test_reLeftQuotient counter = do
    putStrLn "\n── reLeftQuotient (Antimirov partial derivatives) ────────────────"

    -- ── Base case: identity trace ──────────────────────────────────────────────
    -- reLeftQuotient Epsilon r2 = r2  (base case, no derivative taken)
    check counter "ε \\\\ a = a   (nothing consumed, obligation unchanged)" $
        reLeftQuotient Epsilon (Single a) == Single a

    check counter "ε \\\\ ∅ = ∅" $
        reLeftQuotient (Epsilon :: RE Term) Bot == Bot

    check counter "ε \\\\ Σ* = Σ*" $
        reLeftQuotient (Epsilon :: RE Term) (Not Bot) == Not Bot

    -- ── Σ* as the divisor ─────────────────────────────────────────────────────
    -- Quotienting by Σ* asks: what is left of r2 once an /arbitrary/ prefix has
    -- been consumed?  The answer is the suffix closure of L(r2) — neither ∅ nor
    -- Σ*.  Two parts of 'reLeftQuotient' are load-bearing here:
    --
    --   (1) the nullable base case.  ε ∈ L(Σ*), so the whole of r2 is still
    --       owed along the empty-prefix branch and must be unioned in.
    --   (2) 'Wildcard' in the alphabet, standing for "an event other than the
    --       named atoms".  ∂_e(Σ*) = Σ* for every e, so exploration is driven
    --       entirely by the atoms; a pair naming no concrete atom (Σ*, ε, ∅)
    --       would otherwise have no successors at all.
    --
    -- Dropping either one collapses every case below to ∅.

    check counter "Σ* \\\\ ∅ = ∅   (nothing to take a suffix of)" $
        normalize (reLeftQuotient (Not Bot :: RE Term) Bot) == Bot

    check counter "Σ* \\\\ ε = ε   (only the empty prefix lands in ε)" $
        normalize (reLeftQuotient (Not Bot :: RE Term) Epsilon) == Epsilon

    check counter "Σ* \\\\ Σ* = Σ*   (axiom L2: universe stable under quotient)" $
        normalize (reLeftQuotient (Not Bot :: RE Term) (Not Bot)) == Not Bot

    -- suffix closure of {a} = {ε, a}
    check counter "Σ* \\\\ a = {ε, a}" $
        let q = reLeftQuotient (Not Bot) (Single a)
        in matches q [] && matches q [a]
           && not (matches q [b]) && not (matches q [a, a])

    -- suffix closure of {a, b} = {ε, a, b}
    check counter "Σ* \\\\ (a ∨ b) = {ε, a, b}" $
        let q = reLeftQuotient (Not Bot) (Or (Single a) (Single b))
        in matches q [] && matches q [a] && matches q [b]
           && not (matches q [a, b])

    -- suffix closure of {ab} = {ε, b, ab}
    check counter "Σ* \\\\ (a · b) = {ε, b, ab}" $
        let q = reLeftQuotient (Not Bot) (Seq (Single a) (Single b))
        in matches q [] && matches q [b] && matches q [a, b]
           && not (matches q [a]) && not (matches q [b, a])

    -- Termination: before cycle detection, a divisor whose derivative does not
    -- shrink (Σ* and any starred language) made this recursion diverge.
    check counter "Σ* \\\\ F(a) terminates   (cycle detection on ACI-equal pairs)" $
        normalize (reLeftQuotient (Not Bot) (finally a)) == Not Bot

    check counter "(a|b)* \\\\ F(a) terminates" $
        normalize (reLeftQuotient (Star (Or (Single a) (Single b))) (finally a))
            == Not Bot

    -- ── Single-step traces ─────────────────────────────────────────────────────
    -- Exact match: antiDeriv a (Single a) = [ε]; reLeftQuotient ε ε = ε.
    check counter "a \\\\ a = ε   (obligation exactly discharged)" $
        normalize (reLeftQuotient (Single a) (Single a)) == Epsilon

    -- Mismatch: antiDeriv b (Single a) = []; no residual, result is ∅.
    check counter "b \\\\ a = ∅   (disjoint trace and obligation)" $
        normalize (reLeftQuotient (Single b) (Single a)) == Bot

    -- Prefix: antiDeriv a (a·b) = [ε·b] = [b]; reLeftQuotient ε b = b.
    check counter "a \\\\ (a · b) = b   (head consumed, tail remains)" $
        normalize (reLeftQuotient (Single a)
                                 (Seq (Single a) (Single b))) == Single b

    -- Overshoot: ∂_a(a·b) = b; antiDeriv b (Single a) = []; ∅.
    check counter "(a · b) \\\\ a = ∅   (trace overshoots obligation)" $
        normalize (reLeftQuotient (Seq (Single a) (Single b))
                                 (Single a)) == Bot

    -- ── Multi-step traces ──────────────────────────────────────────────────────
    -- Exact multi-step: each step peels one layer; both sides reduce to ε.
    check counter "(a · b) \\\\ (a · b) = ε   (multi-step exact match)" $
        normalize (reLeftQuotient (Seq (Single a) (Single b))
                                 (Seq (Single a) (Single b))) == Epsilon

    -- Partial multi-step: one step consumed, b · c remains.
    check counter "a \\\\ (a · b · c) = b · c   (prefix consumed, tail remains)" $
        normalize (reLeftQuotient (Single a)
                                 (Seq (Single a) (Seq (Single b) (Single c))))
            == Seq (Single b) (Single c)

    -- ── Or in r2: Antimirov splits branches independently ─────────────────────
    -- antiDeriv a (a ∨ b) = [ε] ∪ [] = [ε]: only the a-branch fires.
    -- The b-branch contributes nothing; result is ε.
    check counter "a \\\\ (a ∨ b) = ε   (only the matching Or-branch yields a residual)" $
        normalize (reLeftQuotient (Single a)
                                 (Or (Single a) (Single b))) == Epsilon

    -- antiDeriv a (a ∨ (a·b)) = [ε] ∪ [b] = [ε, b]: BOTH branches fire.
    -- Antimirov yields two separate residuals; their union is ε ∨ b,
    -- meaning the continuation either satisfies the obligation immediately (ε)
    -- or must still perform b (a·b-branch).
    -- Compared as a language: the Or-branches may be accumulated in either
    -- order, so structural equality would over-specify the result.
    check counter "a \\\\ (a ∨ (a · b)) = ε ∨ b   (Antimirov splits two matching Or-branches)" $
        let q = reLeftQuotient (Single a)
                               (Or (Single a) (Seq (Single a) (Single b)))
        in matches q [] && matches q [b]
           && not (matches q [a]) && not (matches q [b, b])

    -- ── Seq with nullable head: Antimirov nullable split ──────────────────────
    -- antiDeriv a (a*·b):
    --   left  = {a*·b}   (a consumed from a*; loop back)
    --   right = ∅        (nullable a*, but antiDeriv a b = []; no split contribution)
    check counter "a \\\\ (a* · b) = a* · b   (one step of a* consumed; loop continues)" $
        normalize (reLeftQuotient (Single a)
                                 (Seq (Star (Single a)) (Single b)))
            == Seq (Star (Single a)) (Single b)

    -- antiDeriv b (a*·b):
    --   left  = ∅        (b cannot start a*, so no left residuals)
    --   right = [ε]      (nullable a* triggers the split; antiDeriv b b = [ε])
    -- The nullable split produces residual ε: a* was skipped entirely.
    check counter "b \\\\ (a* · b) = ε   (nullable a* skipped; b discharged via nullable split)" $
        normalize (reLeftQuotient (Single b)
                                 (Seq (Star (Single a)) (Single b)))
            == Epsilon

-- ── ltl_to_re ─────────────────────────────────────────────────────────────────
-- Algebraic translation LTLf → RE.
-- Returns Nothing when the LTL formula contains a temporal operator on the
-- left-hand side of LTLUntil (no single-step projection exists).

test_ltl_to_re :: IORef Int -> IO ()
test_ltl_to_re counter = do
    putStrLn "\n── ltl_to_re ────────────────────────────────────────────────────"

    -- Base cases
    check counter "ltl_to_re LTLTrue  = Just Σ*" $
        ltlToRe (LTLTrue :: LTL Term) == Just (Not Bot)

    check counter "ltl_to_re LTLFalse = Just ∅" $
        ltlToRe (LTLFalse :: LTL Term) == Just Bot

    check counter "ltl_to_re (LTLAtom a) = Just (Single a)" $
        ltlToRe (LTLAtom a) == Just (Single a)

    -- Negation
    check counter "ltl_to_re (LTLNot (LTLAtom a)) = Just (¬a)" $
        ltlToRe (LTLNot (LTLAtom a)) == Just (Not (Single a))

    check counter "ltl_to_re (LTLNot LTLTrue) = Just (¬Σ*)   (= ∅ after normalization)" $
        ltlToRe (LTLNot (LTLTrue :: LTL Term)) == Just (Not (Not Bot))

    check counter "ltl_to_re (LTLNot LTLFalse) = Just Σ*" $
        ltlToRe (LTLNot (LTLFalse :: LTL Term)) == Just (Not Bot)

    -- Conjunction and disjunction
    check counter "ltl_to_re (LTLAnd (LTLAtom a) (LTLAtom b)) = Just (a ∧ b)" $
        ltlToRe (LTLAnd (LTLAtom a) (LTLAtom b)) == Just (And (Single a) (Single b))

    check counter "ltl_to_re (LTLOr (LTLAtom a) (LTLAtom b)) = Just (a ∨ b)" $
        ltlToRe (LTLOr (LTLAtom a) (LTLAtom b)) == Just (Or (Single a) (Single b))

    -- Next: LTLNext φ ≡ Σ · ⟦φ⟧
    check counter "ltl_to_re (LTLNext (LTLAtom a)) = Just (_ · a)" $
        ltlToRe (LTLNext (LTLAtom a)) == Just (Seq (Single Wildcard) (Single a))

    check counter "ltl_to_re (LTLNext (LTLNext (LTLAtom a))) = Just (_ · (_ · a))   (nested Next)" $
        ltlToRe (LTLNext (LTLNext (LTLAtom a)))
            == Just (Seq (Single Wildcard) (Seq (Single Wildcard) (Single a)))

    -- Finally: LTLFinally φ ≡ Σ* · ⟦φ⟧
    check counter "ltl_to_re (LTLFinally (LTLAtom a)) = Just (Σ* · a)" $
        ltlToRe (LTLFinally (LTLAtom a)) == Just (Seq (Not Bot) (Single a))

    check counter "ltl_to_re (LTLFinally LTLTrue) = Just (Σ* · Σ*)" $
        ltlToRe (LTLFinally (LTLTrue :: LTL Term)) == Just (Seq (Not Bot) (Not Bot))

    check counter "ltl_to_re (LTLFinally LTLFalse) = Just (Σ* · ∅)" $
        ltlToRe (LTLFinally (LTLFalse :: LTL Term)) == Just (Seq (Not Bot) Bot)

    -- Globally: LTLGlobally φ ≡ ¬(Σ* · ¬⟦φ⟧)
    check counter "ltl_to_re (LTLGlobally (LTLAtom a)) = Just (¬(Σ* · ¬a))" $
        ltlToRe (LTLGlobally (LTLAtom a)) == Just (Not (Seq (Not Bot) (Not (Single a))))

    check counter "ltl_to_re (LTLGlobally LTLTrue) = Just (¬(Σ* · ¬Σ*))" $
        ltlToRe (LTLGlobally (LTLTrue :: LTL Term)) == Just (Not (Seq (Not Bot) (Not (Not Bot))))

    -- Until: LTLUntil l1 l2 ≡ step(l1)* · ⟦l2⟧
    -- toSingleStep (LTLAtom a) = Just (Single a)
    check counter "ltl_to_re (LTLAtom a `Until` LTLAtom b) = Just (a* · b)" $
        ltlToRe (LTLUntil (LTLAtom a) (LTLAtom b))
            == Just (Seq (Star (Single a)) (Single b))

    -- toSingleStep LTLTrue = Just (Single Wildcard)
    check counter "ltl_to_re (LTLTrue `Until` LTLAtom b) = Just (_* · b)" $
        ltlToRe (LTLUntil LTLTrue (LTLAtom b))
            == Just (Seq (Star (Single Wildcard)) (Single b))

    -- toSingleStep LTLFalse = Just Bot
    check counter "ltl_to_re (LTLFalse `Until` LTLAtom b) = Just (∅* · b)   (= ε · b = b)" $
        ltlToRe (LTLUntil LTLFalse (LTLAtom b))
            == Just (Seq (Star Bot) (Single b))

    -- toSingleStep returns Nothing for temporal operators → whole Until returns Nothing
    check counter "ltl_to_re (LTLNext _ `Until` LTLAtom b) = Nothing   (no single-step projection)" $
        isNothing (ltlToRe (LTLUntil (LTLNext (LTLAtom a)) (LTLAtom b)))

    check counter "ltl_to_re (LTLFinally _ `Until` LTLAtom b) = Nothing" $
        isNothing (ltlToRe (LTLUntil (LTLFinally (LTLAtom a)) (LTLAtom b)))

    check counter "ltl_to_re (LTLGlobally _ `Until` LTLAtom b) = Nothing" $
        isNothing (ltlToRe (LTLUntil (LTLGlobally (LTLAtom a)) (LTLAtom b)))

    -- Membership tests via iterated derivative + nullability
    let Just reAtomA   = ltlToRe (LTLAtom a)
        Just reNextA   = ltlToRe (LTLNext (LTLAtom a))
        Just reAUntilB = ltlToRe (LTLUntil (LTLAtom a) (LTLAtom b))
        Just reFinallyA = ltlToRe (LTLFinally (LTLAtom a))

    -- LTLAtom a → Single a: matches only word [a]
    check counter "word [a] ∈ ⟦LTLAtom a⟧" $
        matches reAtomA [a]

    check counter "word [b] ∉ ⟦LTLAtom a⟧" $
        not (matches reAtomA [b])

    -- LTLNext (LTLAtom a) → _ · a: matches any two-event word ending in a
    check counter "word [b, a] ∈ ⟦LTLNext (LTLAtom a)⟧" $
        matches reNextA [b, a]

    check counter "word [a] ∉ ⟦LTLNext (LTLAtom a)⟧   (too short)" $
        not (matches reNextA [a])

    -- LTLUntil (LTLAtom a) (LTLAtom b) → a* · b: matches [a,a,b], not [a,a,a]
    check counter "word [a, a, b] ∈ ⟦a U b⟧" $
        matches reAUntilB [a, a, b]

    check counter "word [a, a, a] ∉ ⟦a U b⟧" $
        not (matches reAUntilB [a, a, a])

    -- LTLFinally (LTLAtom a) → Σ* · a: matches [b, b, a], not [b, b, b]
    check counter "word [b, b, a] ∈ ⟦F a⟧" $
        matches reFinallyA [b, b, a]

    check counter "word [b, b, b] ∉ ⟦F a⟧" $
        not (matches reFinallyA [b, b, b])

-- ── Pledge monad: pre field ────────────────────────────────────────────────
-- The combined precondition uses /\ (intersection), not <> (concatenation):
--
--   pre (e >>= f) = pre e /\ (post e \\ pre fe)
--
-- /\ requires BOTH constraints to hold simultaneously in the same history.
-- When the residual (post e \\ pre fe) = ε, the outcome depends on whether
-- pre e is nullable (i.e. whether ε ∈ L(pre e)):
--
--   pre e = Σ*    (nullable):  Σ* /\ ε  = ε    (can run from empty start)
--   pre e = a     (non-nullable): {a} /\ ε = ∅  (impossible: history ≠ ε)
--
-- The second case is the key insight: even though post e perfectly covers
-- pre fe, the conjunction {a} ∩ {ε} = ∅ correctly flags the contradiction
-- — no history can simultaneously contain event a AND be empty.

test_effectful :: IORef Int -> IO ()
test_effectful counter = do
    putStrLn "\n── Pledge: pre /\\ (post \\\\ pre) ────────────────────────────────"

    -- post e = ε (produces nothing): residual = ε \\ pre fe = pre fe (base case).
    -- pre = universe /\ pre fe = pre fe  (universe is identity for /\).
    let e0  = Pledge $ return ((), universe, empty,    universe) :: Pledge IO (RE Term) ()
        fe0 = Pledge $ return ((), Single a, empty,    universe) :: Pledge IO (RE Term) ()
    (_, pre0, _, _) <- runPledge (e0 >> fe0)
    check counter "pre (e{post=ε} >>= \\_ -> fe{pre=a}) = a   (nothing produced; full pre fe remains)" $
        normalize pre0 == Single a

    -- post e = Single a, pre fe = Single a: post exactly covers pre fe.
    -- residual = a \\ a = ε.
    -- pre = universe /\ ε = ε   (isTop universe → right side = ε).
    let e1  = Pledge $ return ((), universe,  Single a, universe) :: Pledge IO (RE Term) ()
        fe1 = Pledge $ return ((), Single a,  empty,    universe) :: Pledge IO (RE Term) ()
    (_, pre1, _, _) <- runPledge (e1 >> fe1)
    check counter "pre (e{pre=Σ*,post=a} >>= \\_ -> fe{pre=a}) = ε   (Σ* /\\ ε = ε)" $
        normalize pre1 == Epsilon

    -- pre e = Single a, post e = Single b, pre fe = Single b:
    -- residual = b \\ b = ε.
    -- pre = Single a /\ ε = And (Single a) Epsilon.
    -- nullable (Single a) = False  →  {a} ∩ {ε} = ∅ = Bot.
    -- Correct: the history cannot simultaneously be "contains a" and "is empty".
    let e2  = Pledge $ return ((), Single a,  Single b, universe) :: Pledge IO (RE Term) ()
        fe2 = Pledge $ return ((), Single b,  empty,    universe) :: Pledge IO (RE Term) ()
    (_, pre2, _, _) <- runPledge (e2 >> fe2)
    check counter "pre (e{pre=a,post=b} >>= \\_ -> fe{pre=b}) = ∅   ({a} /\\ ε = ∅; contradictory constraints)" $
        normalize pre2 == Bot

    -- post e = Single a, pre fe = Single b (a ≠ b): residual = a \\ b = ∅.
    -- pre = universe /\ ∅ = ∅  (Bot absorbs).
    let e3  = Pledge $ return ((), universe,  Single a, universe) :: Pledge IO (RE Term) ()
        fe3 = Pledge $ return ((), Single b,  empty,    universe) :: Pledge IO (RE Term) ()
    (_, pre3, _, _) <- runPledge (e3 >> fe3)
    check counter "pre (e{post=a} >>= \\_ -> fe{pre=b}) = ∅   (a ≠ b; post does not cover pre fe)" $
        normalize pre3 == Bot

-- ── Pledge SL ──────────────────────────────────────────────────────────────
-- Mirrors test_effectful but with SL as the effect type.
-- concatenation = SepStar, conjunction = Conj, empty = Emp, universe = Top,
-- leftQuotient base cases: Emp\q = q,  Top\_ = Emp,  general = Wand p q.

test_effectful_sl :: IORef Int -> IO ()
test_effectful_sl counter = do
    putStrLn "\n── Pledge SL ─────────────────────────────────────────────────────"

    -- ── pre: post = Emp, nothing provided ───────────────────────────────────────
    -- leftQuotient Emp (Cell 0 42) = Cell 0 42   (base case: Emp\q = q)
    -- pre = Top /\ Cell 0 42 = Conj Top (Cell 0 42)
    let e0  = Pledge $ return ((), Top,       Emp,       Top) :: Pledge IO SL ()
        fe0 = Pledge $ return ((), Cell 0 42, Emp,       Top) :: Pledge IO SL ()
    (_, pre0, _, _) <- runPledge (e0 >> fe0)
    check counter "pre (e{post=Emp} >>= fe{pre=Cell 0 42}) = Conj Top (Cell 0 42)   (nothing provided; full pre fe remains)" $
        pre0 == Conj Top (Cell 0 42)

    -- ── pre: post = Top, all preconditions discharged ───────────────────────────
    -- leftQuotient Top (Cell 0 42) = Emp          (base case: Top\_ = Emp)
    -- pre = Top /\ Emp = Conj Top Emp
    let e1  = Pledge $ return ((), Top,       Top,       Top) :: Pledge IO SL ()
        fe1 = Pledge $ return ((), Cell 0 42, Emp,       Top) :: Pledge IO SL ()
    (_, pre1, _, _) <- runPledge (e1 >> fe1)
    check counter "pre (e{post=Top} >>= fe{pre=Cell 0 42}) = Conj Top Emp   (Top discharges any precondition)" $
        pre1 == Conj Top Emp

    -- ── post: SepStar combines disjoint heap ownership ──────────────────────────
    -- e writes Cell 0 42, fe writes Cell 1 99 (disjoint addresses).
    -- post combined = SepStar (Cell 0 42) (Cell 1 99)
    let e2  = Pledge $ return ((), Top, Cell 0 42, Top) :: Pledge IO SL ()
        fe2 = Pledge $ return ((), Top, Cell 1 99, Top) :: Pledge IO SL ()
    (_, _, post2, _) <- runPledge (e2 >> fe2)
    check counter "post (write{0} >> write{1}) = SepStar (Cell 0 42) (Cell 1 99)   (disjoint ownership)" $
        post2 == SepStar (Cell 0 42) (Cell 1 99)

    -- ── future: obligation not discharged when post fe = Emp ────────────────────
    -- e has future = Cell 0 42 (must eventually hold).
    -- leftQuotient Emp (Cell 0 42) = Cell 0 42    (fe produced nothing toward it)
    -- future combined = Cell 0 42 /\ Top = Conj (Cell 0 42) Top
    let e3  = Pledge $ return ((), Top, Emp, Cell 0 42) :: Pledge IO SL ()
        fe3 = Pledge $ return ((), Top, Emp, Top)       :: Pledge IO SL ()
    (_, _, _, fut3) <- runPledge (e3 >> fe3)
    check counter "future (e{future=Cell 0 42} >>= fe{post=Emp}) = Conj (Cell 0 42) Top   (obligation outstanding)" $
        fut3 == Conj (Cell 0 42) Top

    -- ── future: obligation discharged when post fe = Top ────────────────────────
    -- leftQuotient Top (Cell 0 42) = Emp          (Top covers everything)
    -- future combined = Emp /\ Top = Conj Emp Top
    let e4  = Pledge $ return ((), Top, Emp, Cell 0 42) :: Pledge IO SL ()
        fe4 = Pledge $ return ((), Top, Top, Top)       :: Pledge IO SL ()
    (_, _, _, fut4) <- runPledge (e4 >> fe4)
    check counter "future (e{future=Cell 0 42} >>= fe{post=Top}) = Conj Emp Top   (obligation discharged)" $
        fut4 == Conj Emp Top

    -- ── pre: Pure constraint propagates through bind ─────────────────────────────
    -- fe requires h[0] > 5 AND spatial ownership of Cell 0 42.
    -- post e = Emp → residual = full pre fe   (base case)
    -- pre combined = Top /\ Conj (Pure _) (Cell 0 42)
    let gtFive = PGt (ValAt 0) (Lit 5)
        e5  = Pledge $ return ((), Top,                              Emp, Top) :: Pledge IO SL ()
        fe5 = Pledge $ return ((), Conj (Pure gtFive) (Cell 0 42),  Emp, Top) :: Pledge IO SL ()
    (_, pre5, _, _) <- runPledge (e5 >> fe5)
    check counter "pre (e{post=Emp} >>= fe{pre=⌈h[0]>5⌉∧Cell 0 42}) = Conj Top (Conj (Pure _) (Cell 0 42))" $
        pre5 == Conj Top (Conj (Pure gtFive) (Cell 0 42))

-- ── normalizeSL ───────────────────────────────────────────────────────────────

test_normalizeSL :: IORef Int -> IO ()
test_normalizeSL counter = do
    putStrLn "\n── normalizeSL ──────────────────────────────────────────────────────"

    -- SepStar: Emp is unit
    check counter "Emp * P = P" $
        normalizeSL (SepStar Emp (Cell 0 42)) == Cell 0 42

    check counter "P * Emp = P" $
        normalizeSL (SepStar (Cell 0 42) Emp) == Cell 0 42

    check counter "Emp * Emp = Emp" $
        normalizeSL (SepStar Emp Emp) == Emp

    -- SepStar: non-trivial terms are preserved
    check counter "Cell 0 42 * Cell 1 99 unchanged" $
        normalizeSL (SepStar (Cell 0 42) (Cell 1 99)) == SepStar (Cell 0 42) (Cell 1 99)

    -- SepStar: recursive normalisation
    check counter "Emp * (Emp * Cell 0 42) = Cell 0 42   (nested)" $
        normalizeSL (SepStar Emp (SepStar Emp (Cell 0 42))) == Cell 0 42

    -- Conj: Top is unit
    check counter "⊤ ∧ P = P" $
        normalizeSL (Conj Top (Cell 0 42)) == Cell 0 42

    check counter "P ∧ ⊤ = P" $
        normalizeSL (Conj (Cell 0 42) Top) == Cell 0 42

    check counter "⊤ ∧ ⊤ = ⊤" $
        normalizeSL (Conj Top Top) == Top

    -- Conj: idempotent
    check counter "P ∧ P = P" $
        normalizeSL (Conj (Cell 0 42) (Cell 0 42)) == Cell 0 42

    check counter "Emp ∧ Emp = Emp" $
        normalizeSL (Conj Emp Emp) == Emp

    -- Conj: recursive normalisation
    check counter "⊤ ∧ (⊤ ∧ Cell 0 42) = Cell 0 42   (nested)" $
        normalizeSL (Conj Top (Conj Top (Cell 0 42))) == Cell 0 42

    -- Wand: Emp -* Q = Q
    check counter "Emp -* Q = Q" $
        normalizeSL (Wand Emp (Cell 0 42)) == Cell 0 42

    -- Wand: P -* ⊤ = ⊤
    check counter "P -* ⊤ = ⊤" $
        normalizeSL (Wand (Cell 0 42) Top) == Top

    check counter "Emp -* ⊤ = ⊤   (both rules apply; Emp rule fires first)" $
        normalizeSL (Wand Emp Top) == Top

    -- Wand: non-trivial terms are preserved
    check counter "Cell 0 42 -* Cell 1 99 unchanged" $
        normalizeSL (Wand (Cell 0 42) (Cell 1 99)) == Wand (Cell 0 42) (Cell 1 99)

    -- Wand: recursive normalisation
    check counter "(Emp * Cell 0 42) -* ⊤ = ⊤   (inner SepStar normalised first)" $
        normalizeSL (Wand (SepStar Emp (Cell 0 42)) Top) == Top

    -- Pure: passes through unchanged
    check counter "Pure p unchanged" $
        normalizeSL (Pure (PGt (ValAt 0) (Lit 5))) == Pure (PGt (ValAt 0) (Lit 5))

    -- Pure with Conj: Top stripped
    check counter "⊤ ∧ Pure p = Pure p" $
        normalizeSL (Conj Top (Pure (PEq (ValAt 0) (Lit 0)))) == Pure (PEq (ValAt 0) (Lit 0))

-- ── Main ──────────────────────────────────────────────────────────────────────

main :: IO ()
main = do
    putStrLn "=== Future.hs unit tests ==================================================="
    counter <- newIORef (0 :: Int)
    test_subsumesEvent counter
    test_nullable      counter
    test_derivative    counter
    test_atoms         counter
    test_first         counter
    test_normalize     counter
    test_reLeftQuotient counter
    test_effectful     counter
    test_ltl_to_re     counter
    test_effectful_sl  counter
    test_normalizeSL   counter
    n <- readIORef counter
    putStrLn $ "\n=== All " ++ show n ++ " assertions passed =================================="
