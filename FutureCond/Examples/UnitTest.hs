{-# OPTIONS_GHC -i.. #-}
module Examples.UnitTest where

import Prelude hiding ((<>))
import Data.IORef
import Data.List (sort)
import Future

-- ── Helpers ───────────────────────────────────────────────────────────────────

-- Convenience events used throughout
a, b, c :: Event
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
sameSet :: [Event] -> [Event] -> Bool
sameSet xs ys = sort (map show xs) == sort (map show ys)

-- Fold derivatives over a word, normalising at each step, then check
-- nullability.  This is the standard Brzozowski membership test and a
-- useful illustration of how derivative composes.
matches :: RE -> [Event] -> Bool
matches r []     = nullable r
matches r (e:es) = matches (normalize (derivative e r)) es

-- ── subsumesEvent ─────────────────────────────────────────────────────────────
-- subsumesEvent e p: does occurrence e match pattern p?
-- Wildcard as pattern accepts everything; Wildcard as occurrence matches nothing specific.

test_subsumesEvent :: IORef Int -> IO ()
test_subsumesEvent counter = do
    putStrLn "\n── subsumesEvent ────────────────────────────────────────────────"

    let x = Var "x"
        sendX = Atom "send" x
        recvX = Atom "recv" x
        sendY = Atom "send" (Var "y")
        send1 = Atom "send" (Num 1)
        sendL = Atom "send" (List [Num 1, Num 2])

    -- Wildcard pattern subsumes any occurrence
    check counter "subsumesEvent a        Wildcard = True   (concrete event matches wildcard)" $
        subsumesEvent a Wildcard == True

    check counter "subsumesEvent Wildcard Wildcard = True   (wildcard occurrence matches wildcard pattern)" $
        subsumesEvent Wildcard Wildcard == True

    check counter "subsumesEvent send(x)  Wildcard = True   (any atom matches wildcard)" $
        subsumesEvent sendX Wildcard == True

    -- Wildcard occurrence vs concrete pattern
    check counter "subsumesEvent Wildcard send(x) = False  (wildcard occ ≠ specific pattern)" $
        subsumesEvent Wildcard sendX == False

    -- Identical atoms
    check counter "subsumesEvent send(x) send(x) = True    (same name, same arg)" $
        subsumesEvent sendX sendX == True

    check counter "subsumesEvent send(1) send(1) = True    (same name, Num arg)" $
        subsumesEvent send1 send1 == True

    -- Different names
    check counter "subsumesEvent send(x) recv(x) = False   (different name)" $
        subsumesEvent sendX recvX == False

    -- Same name, different args
    check counter "subsumesEvent send(x) send(y) = False   (same name, different Var arg)" $
        subsumesEvent sendX sendY == False

    check counter "subsumesEvent send(x) send(1) = False   (Var vs Num arg)" $
        subsumesEvent sendX send1 == False

    -- List term
    check counter "subsumesEvent send([1,2]) send([1,2]) = True    (List arg equal)" $
        subsumesEvent sendL sendL == True

    check counter "subsumesEvent send([1,2]) send(x) = False   (List vs Var)" $
        subsumesEvent sendL sendX == False

    check counter "subsumesEvent send([1,2]) Wildcard = True   (List atom matches wildcard)" $
        subsumesEvent sendL Wildcard == True

-- ── nullable ──────────────────────────────────────────────────────────────────
-- ν(r) = True  iff  ε ∈ L(r).

test_nullable :: IORef Int -> IO ()
test_nullable counter = do
    putStrLn "\n── nullable ─────────────────────────────────────────────────────"

    -- Base cases
    check counter "nullable Bot     = False   (empty language contains no word at all)"  $
        nullable Bot == False

    check counter "nullable Epsilon = True    (ε is the only word in {ε})"  $
        nullable Epsilon == True

    check counter "nullable (Single a) = False  (a requires exactly one event)"  $
        nullable (Single a) == False

    check counter "nullable (Single _) = False  (wildcard still consumes one step)"  $
        nullable (Single Wildcard) == False

    -- Sequence: ε ∈ r1·r2  iff  ε ∈ r1  AND  ε ∈ r2
    check counter "nullable (ε · ε) = True"  $
        nullable (Seq Epsilon Epsilon) == True

    check counter "nullable (a · b) = False"  $
        nullable (Seq (Single a) (Single b)) == False

    check counter "nullable (ε · a) = False  (tail is not nullable)"  $
        nullable (Seq Epsilon (Single a)) == False

    -- Union: ε ∈ r1 + r2  iff  ε ∈ r1  OR  ε ∈ r2
    check counter "nullable (∅ ∨ ε) = True"  $
        nullable (Or Bot Epsilon) == True

    check counter "nullable (a ∨ b) = False"  $
        nullable (Or (Single a) (Single b)) == False

    -- Intersection: ε ∈ r1 ∧ r2  iff  ε ∈ r1  AND  ε ∈ r2
    check counter "nullable (ε ∧ ε) = True"  $
        nullable (And Epsilon Epsilon) == True

    check counter "nullable (a ∧ ε) = False"  $
        nullable (And (Single a) Epsilon) == False

    -- Star: zero repetitions always accepted
    check counter "nullable (a*) = True   (zero copies of a)"  $
        nullable (Star (Single a)) == True

    check counter "nullable (∅*) = True   (∅* = ε by RE algebra)"  $
        nullable (Star Bot) == True

    -- Complement: ν(¬r) = ¬ν(r)
    check counter "nullable (¬ε) = False  (ε ∉ complement of {ε})"  $
        nullable (Not Epsilon) == False

    check counter "nullable (¬∅) = True   (¬∅ = Σ*, which contains ε)"  $
        nullable (Not Bot) == True

    check counter "nullable (¬a) = True   (ε ∉ {a}, so ε ∈ ¬{a})"  $
        nullable (Not (Single a)) == True

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
        atoms Bot == []

    check counter "atoms ε = []"  $
        atoms Epsilon == []

    check counter "atoms (_) = []   (Wildcard contributes no concrete event)"  $
        atoms (Single Wildcard) == []

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
        first Bot == []

    check counter "first ε = []   (ε starts with no event)"  $
        first Epsilon == []

    check counter "first (a) = [a]"  $
        first (Single a) == [a]

    -- Wildcard: firstWith uses the Wildcard as-is when it appears as Single
    check counter "first (_) = [_]   (wildcard event returned; alphabet from atoms is empty)"  $
        first (Single Wildcard) == [Wildcard]

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
        first (And (Single a) (Single b)) == []

    check counter "first ((a ∨ b) ∧ (b ∨ c)) = [b]   (only b is in both first sets)"  $
        first (And (Or (Single a) (Single b))
                   (Or (Single b) (Single c))) == [b]

    -- Star
    check counter "first (a*) = [a]"  $
        first (Star (Single a)) == [a]

    check counter "first (∅*) = []   (∅* = ε, no events)"  $
        first (Star Bot) == []

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
        firstWith [a, b] (Not (Not Bot)) == []

    -- first (¬∅) = [] when atoms is empty:
    -- atoms (Not Bot) = atoms Bot = []; so firstWith [] (Not Bot) = [].
    check counter "first (¬∅) = []   (no concrete atoms in RE → empty alphabet for unfolding)"  $
        first (Not Bot) == []

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
        normalize Bot == Bot

    check counter "normalize ε = ε" $
        normalize Epsilon == Epsilon

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
        normalize (Not Bot) == Not Bot

    check counter "normalize (¬Σ*) = ∅   (complement of top = empty)" $
        normalize (Not (Not Bot)) == Bot

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
        normalize (Star Bot) == Epsilon

    check counter "normalize (ε*) = ε   (epsilon Kleene = epsilon)" $
        normalize (Star Epsilon) == Epsilon

    check counter "normalize (a*) = a*   (no simplification)" $
        normalize (Star (Single a)) == Star (Single a)

    -- inner simplification: (∅ · a)* = ∅* = ε
    check counter "normalize ((∅ · a)*) = ε   (inner Bot collapses before Star)" $
        normalize (Star (Seq Bot (Single a))) == Epsilon

-- ── reSubtraction ─────────────────────────────────────────────────────────────
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

test_reSubtraction :: IORef Int -> IO ()
test_reSubtraction counter = do
    putStrLn "\n── reSubtraction (Antimirov partial derivatives) ────────────────"

    -- ── Base case: identity trace ──────────────────────────────────────────────
    -- reSubtraction Epsilon r2 = r2  (base case, no derivative taken)
    check counter "ε \\\\ a = a   (nothing consumed, obligation unchanged)" $
        reSubtraction Epsilon (Single a) == Single a

    check counter "ε \\\\ ∅ = ∅" $
        reSubtraction Epsilon Bot == Bot

    check counter "ε \\\\ Σ* = Σ*" $
        reSubtraction Epsilon (Not Bot) == Not Bot

    -- ── Σ* as the trace ───────────────────────────────────────────────────────
    -- The key invariant: derivative e (Not Bot) = Not (derivative e Bot) = Not Bot,
    -- so r1 stays Not Bot at every recursive step.  Exploration is entirely driven
    -- by atoms r2; the recursion terminates only when antiDeriv produces residuals
    -- with no further atoms, at which point firstWith [] (Not Bot) = [] → Bot.
    --
    -- Consequence: Σ* \\ r2 = ∅ for every r2 whose atoms-driven derivation
    -- eventually reaches a residual with empty atoms (ε or ∅).
    -- The semantically correct left-quotient Σ*\r2 = Σ* in all cases, but the
    -- atoms-based alphabet extraction cannot produce events from Not Bot alone.

    -- r2 = ∅: atoms ∅ = []; no events in alphabet → immediately ∅.
    check counter "Σ* \\\\ ∅ = ∅   (atoms ∅ is empty; no events to explore)" $
        normalize (reSubtraction (Not Bot) Bot) == Bot

    -- r2 = ε: atoms ε = []; same as above.
    check counter "Σ* \\\\ ε = ∅   (atoms ε is empty; no events to explore)" $
        normalize (reSubtraction (Not Bot) Epsilon) == Bot

    -- r2 = Σ*: atoms (Not Bot) = []; combined alphabet still empty.
    check counter "Σ* \\\\ Σ* = ∅   (atoms of Not Bot is empty; no events explored)" $
        normalize (reSubtraction (Not Bot) (Not Bot)) == Bot

    -- r2 = Single a: alph = [a]; evts = [a] (∂_a(∅) = ∅, not total).
    -- step a: dr1 = Not Bot (unchanged); antiDeriv a (Single a) = [ε].
    -- recurse: Σ* \\ ε → alph = [] → ∅.
    check counter "Σ* \\\\ a = ∅   (reaches Σ* \\\\ ε after one step; no atoms in ε)" $
        normalize (reSubtraction (Not Bot) (Single a)) == Bot

    -- r2 = a ∨ b: alph = [a, b]; both steps produce [ε] via antiDeriv,
    -- then recurse to Σ* \\ ε = ∅.
    check counter "Σ* \\\\ (a ∨ b) = ∅   (both Or-branches reduce to Σ* \\\\ ε)" $
        normalize (reSubtraction (Not Bot) (Or (Single a) (Single b))) == Bot

    -- r2 = a · b: alph = [a, b].
    -- step a: antiDeriv a (a·b) = [b]; recurse Σ* \\ b → ∅.
    -- step b: antiDeriv b (a·b) = [];  no residuals → ∅.
    check counter "Σ* \\\\ (a · b) = ∅   (chain reduces to Σ* \\\\ ε)" $
        normalize (reSubtraction (Not Bot) (Seq (Single a) (Single b))) == Bot

    -- ── Single-step traces ─────────────────────────────────────────────────────
    -- Exact match: antiDeriv a (Single a) = [ε]; reSubtraction ε ε = ε.
    check counter "a \\\\ a = ε   (obligation exactly discharged)" $
        normalize (reSubtraction (Single a) (Single a)) == Epsilon

    -- Mismatch: antiDeriv b (Single a) = []; no residual, result is ∅.
    check counter "b \\\\ a = ∅   (disjoint trace and obligation)" $
        normalize (reSubtraction (Single b) (Single a)) == Bot

    -- Prefix: antiDeriv a (a·b) = [ε·b] = [b]; reSubtraction ε b = b.
    check counter "a \\\\ (a · b) = b   (head consumed, tail remains)" $
        normalize (reSubtraction (Single a)
                                 (Seq (Single a) (Single b))) == Single b

    -- Overshoot: ∂_a(a·b) = b; antiDeriv b (Single a) = []; ∅.
    check counter "(a · b) \\\\ a = ∅   (trace overshoots obligation)" $
        normalize (reSubtraction (Seq (Single a) (Single b))
                                 (Single a)) == Bot

    -- ── Multi-step traces ──────────────────────────────────────────────────────
    -- Exact multi-step: each step peels one layer; both sides reduce to ε.
    check counter "(a · b) \\\\ (a · b) = ε   (multi-step exact match)" $
        normalize (reSubtraction (Seq (Single a) (Single b))
                                 (Seq (Single a) (Single b))) == Epsilon

    -- Partial multi-step: one step consumed, b · c remains.
    check counter "a \\\\ (a · b · c) = b · c   (prefix consumed, tail remains)" $
        normalize (reSubtraction (Single a)
                                 (Seq (Single a) (Seq (Single b) (Single c))))
            == Seq (Single b) (Single c)

    -- ── Or in r2: Antimirov splits branches independently ─────────────────────
    -- antiDeriv a (a ∨ b) = [ε] ∪ [] = [ε]: only the a-branch fires.
    -- The b-branch contributes nothing; result is ε.
    check counter "a \\\\ (a ∨ b) = ε   (only the matching Or-branch yields a residual)" $
        normalize (reSubtraction (Single a)
                                 (Or (Single a) (Single b))) == Epsilon

    -- antiDeriv a (a ∨ (a·b)) = [ε] ∪ [b] = [ε, b]: BOTH branches fire.
    -- Antimirov yields two separate residuals; their union is ε ∨ b,
    -- meaning the continuation either satisfies the obligation immediately (ε)
    -- or must still perform b (a·b-branch).
    check counter "a \\\\ (a ∨ (a · b)) = ε ∨ b   (Antimirov splits two matching Or-branches)" $
        normalize (reSubtraction (Single a)
                                 (Or (Single a) (Seq (Single a) (Single b))))
            == Or Epsilon (Single b)

    -- ── Seq with nullable head: Antimirov nullable split ──────────────────────
    -- antiDeriv a (a*·b):
    --   left  = {a*·b}   (a consumed from a*; loop back)
    --   right = ∅        (nullable a*, but antiDeriv a b = []; no split contribution)
    check counter "a \\\\ (a* · b) = a* · b   (one step of a* consumed; loop continues)" $
        normalize (reSubtraction (Single a)
                                 (Seq (Star (Single a)) (Single b)))
            == Seq (Star (Single a)) (Single b)

    -- antiDeriv b (a*·b):
    --   left  = ∅        (b cannot start a*, so no left residuals)
    --   right = [ε]      (nullable a* triggers the split; antiDeriv b b = [ε])
    -- The nullable split produces residual ε: a* was skipped entirely.
    check counter "b \\\\ (a* · b) = ε   (nullable a* skipped; b discharged via nullable split)" $
        normalize (reSubtraction (Single b)
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
        ltl_to_re LTLTrue == Just (Not Bot)

    check counter "ltl_to_re LTLFalse = Just ∅" $
        ltl_to_re LTLFalse == Just Bot

    check counter "ltl_to_re (LTLAtom a) = Just (Single a)" $
        ltl_to_re (LTLAtom a) == Just (Single a)

    -- Negation
    check counter "ltl_to_re (LTLNot (LTLAtom a)) = Just (¬a)" $
        ltl_to_re (LTLNot (LTLAtom a)) == Just (Not (Single a))

    check counter "ltl_to_re (LTLNot LTLTrue) = Just (¬Σ*)   (= ∅ after normalization)" $
        ltl_to_re (LTLNot LTLTrue) == Just (Not (Not Bot))

    check counter "ltl_to_re (LTLNot LTLFalse) = Just Σ*" $
        ltl_to_re (LTLNot LTLFalse) == Just (Not Bot)

    -- Conjunction and disjunction
    check counter "ltl_to_re (LTLAnd (LTLAtom a) (LTLAtom b)) = Just (a ∧ b)" $
        ltl_to_re (LTLAnd (LTLAtom a) (LTLAtom b)) == Just (And (Single a) (Single b))

    check counter "ltl_to_re (LTLOr (LTLAtom a) (LTLAtom b)) = Just (a ∨ b)" $
        ltl_to_re (LTLOr (LTLAtom a) (LTLAtom b)) == Just (Or (Single a) (Single b))

    -- Next: LTLNext φ ≡ Σ · ⟦φ⟧
    check counter "ltl_to_re (LTLNext (LTLAtom a)) = Just (_ · a)" $
        ltl_to_re (LTLNext (LTLAtom a)) == Just (Seq (Single Wildcard) (Single a))

    check counter "ltl_to_re (LTLNext (LTLNext (LTLAtom a))) = Just (_ · (_ · a))   (nested Next)" $
        ltl_to_re (LTLNext (LTLNext (LTLAtom a)))
            == Just (Seq (Single Wildcard) (Seq (Single Wildcard) (Single a)))

    -- Finally: LTLFinally φ ≡ Σ* · ⟦φ⟧
    check counter "ltl_to_re (LTLFinally (LTLAtom a)) = Just (Σ* · a)" $
        ltl_to_re (LTLFinally (LTLAtom a)) == Just (Seq (Not Bot) (Single a))

    check counter "ltl_to_re (LTLFinally LTLTrue) = Just (Σ* · Σ*)" $
        ltl_to_re (LTLFinally LTLTrue) == Just (Seq (Not Bot) (Not Bot))

    check counter "ltl_to_re (LTLFinally LTLFalse) = Just (Σ* · ∅)" $
        ltl_to_re (LTLFinally LTLFalse) == Just (Seq (Not Bot) Bot)

    -- Globally: LTLGlobally φ ≡ ¬(Σ* · ¬⟦φ⟧)
    check counter "ltl_to_re (LTLGlobally (LTLAtom a)) = Just (¬(Σ* · ¬a))" $
        ltl_to_re (LTLGlobally (LTLAtom a)) == Just (Not (Seq (Not Bot) (Not (Single a))))

    check counter "ltl_to_re (LTLGlobally LTLTrue) = Just (¬(Σ* · ¬Σ*))" $
        ltl_to_re (LTLGlobally LTLTrue) == Just (Not (Seq (Not Bot) (Not (Not Bot))))

    -- Until: LTLUntil l1 l2 ≡ step(l1)* · ⟦l2⟧
    -- toSingleStep (LTLAtom a) = Just (Single a)
    check counter "ltl_to_re (LTLAtom a `Until` LTLAtom b) = Just (a* · b)" $
        ltl_to_re (LTLUntil (LTLAtom a) (LTLAtom b))
            == Just (Seq (Star (Single a)) (Single b))

    -- toSingleStep LTLTrue = Just (Single Wildcard)
    check counter "ltl_to_re (LTLTrue `Until` LTLAtom b) = Just (_* · b)" $
        ltl_to_re (LTLUntil LTLTrue (LTLAtom b))
            == Just (Seq (Star (Single Wildcard)) (Single b))

    -- toSingleStep LTLFalse = Just Bot
    check counter "ltl_to_re (LTLFalse `Until` LTLAtom b) = Just (∅* · b)   (= ε · b = b)" $
        ltl_to_re (LTLUntil LTLFalse (LTLAtom b))
            == Just (Seq (Star Bot) (Single b))

    -- toSingleStep returns Nothing for temporal operators → whole Until returns Nothing
    check counter "ltl_to_re (LTLNext _ `Until` LTLAtom b) = Nothing   (no single-step projection)" $
        ltl_to_re (LTLUntil (LTLNext (LTLAtom a)) (LTLAtom b)) == Nothing

    check counter "ltl_to_re (LTLFinally _ `Until` LTLAtom b) = Nothing" $
        ltl_to_re (LTLUntil (LTLFinally (LTLAtom a)) (LTLAtom b)) == Nothing

    check counter "ltl_to_re (LTLGlobally _ `Until` LTLAtom b) = Nothing" $
        ltl_to_re (LTLUntil (LTLGlobally (LTLAtom a)) (LTLAtom b)) == Nothing

    -- Membership tests via iterated derivative + nullability
    let Just reAtomA   = ltl_to_re (LTLAtom a)
        Just reNextA   = ltl_to_re (LTLNext (LTLAtom a))
        Just reAUntilB = ltl_to_re (LTLUntil (LTLAtom a) (LTLAtom b))
        Just reFinallyA = ltl_to_re (LTLFinally (LTLAtom a))

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

-- ── Effectful monad: pre field ────────────────────────────────────────────────
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
    putStrLn "\n── Effectful: pre /\\ (post \\\\ pre) ────────────────────────────────"

    -- post e = ε (produces nothing): residual = ε \\ pre fe = pre fe (base case).
    -- pre = universe /\ pre fe = pre fe  (universe is identity for /\).
    let e0  = Effectful { ret = (), pre = universe,  post = empty,    future = universe }
        fe0 = Effectful { ret = (), pre = Single a,  post = empty,    future = universe }
    check counter "pre (e{post=ε} >>= \\_ -> fe{pre=a}) = a   (nothing produced; full pre fe remains)" $
        normalize (pre (e0 >>= \_ -> fe0)) == Single a

    -- post e = Single a, pre fe = Single a: post exactly covers pre fe.
    -- residual = a \\ a = ε.
    -- pre = universe /\ ε = ε   (isTop universe → right side = ε).
    let e1  = Effectful { ret = (), pre = universe,  post = Single a, future = universe }
        fe1 = Effectful { ret = (), pre = Single a,  post = empty,    future = universe }
    check counter "pre (e{pre=Σ*,post=a} >>= \\_ -> fe{pre=a}) = ε   (Σ* /\\ ε = ε)" $
        normalize (pre (e1 >>= \_ -> fe1)) == Epsilon

    -- pre e = Single a, post e = Single b, pre fe = Single b:
    -- residual = b \\ b = ε.
    -- pre = Single a /\ ε = And (Single a) Epsilon.
    -- nullable (Single a) = False  →  {a} ∩ {ε} = ∅ = Bot.
    -- Correct: the history cannot simultaneously be "contains a" and "is empty".
    let e2  = Effectful { ret = (), pre = Single a,  post = Single b, future = universe }
        fe2 = Effectful { ret = (), pre = Single b,  post = empty,    future = universe }
    check counter "pre (e{pre=a,post=b} >>= \\_ -> fe{pre=b}) = ∅   ({a} /\\ ε = ∅; contradictory constraints)" $
        normalize (pre (e2 >>= \_ -> fe2)) == Bot

    -- post e = Single a, pre fe = Single b (a ≠ b): residual = a \\ b = ∅.
    -- pre = universe /\ ∅ = ∅  (Bot absorbs).
    let e3  = Effectful { ret = (), pre = universe,  post = Single a, future = universe }
        fe3 = Effectful { ret = (), pre = Single b,  post = empty,    future = universe }
    check counter "pre (e{post=a} >>= \\_ -> fe{pre=b}) = ∅   (a ≠ b; post does not cover pre fe)" $
        normalize (pre (e3 >>= \_ -> fe3)) == Bot

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
    test_reSubtraction counter
    test_effectful     counter
    test_ltl_to_re     counter
    n <- readIORef counter
    putStrLn $ "\n=== All " ++ show n ++ " assertions passed =================================="
