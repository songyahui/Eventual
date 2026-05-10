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
    n <- readIORef counter
    putStrLn $ "\n=== All " ++ show n ++ " assertions passed =================================="
