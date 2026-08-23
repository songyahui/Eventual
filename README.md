# Pledge

A Haskell library for monadic programs with **runtime temporal specifications**.
Each effectful action carries a precondition, a postcondition, and a *future
obligation* — all composed automatically through monadic sequencing.

## Motivation

Pre- and post-conditions reason about a single call boundary.
Many correctness properties are **deferred obligations** — things that must
happen at some later, unknown point:

- Every `malloc` must eventually be followed by a `free`
- Every `open` must eventually be followed by a `close`
- Every `beginTx` must eventually reach a `commit` or `rollback`

`Pledge` makes these obligations first-class values that propagate
automatically through `>>=`.

---

## The `Pledge` Monad

```haskell
newtype Pledge m eff a = Pledge { runPledge :: m (a, eff, eff, eff) }
--                                               ret  pre  post  fut
```

A `Pledge m eff a` wraps an `m`-action that produces four things at once:

| Component | Type | Meaning |
|---|---|---|
| `ret` | `a` | return value |
| `pre` | `eff` | what must have held immediately before this action |
| `post` | `eff` | what this action emits / produces |
| `fut` | `eff` | obligation that must be discharged by subsequent actions |

Because all four components come from a **single run** of the underlying action,
stateful resources (file handles, heap addresses, …) are allocated exactly once.
Data-dependent futures arise naturally: `ret` is in scope when the programmer
constructs `fut` inside the `m` action.

```haskell
-- The future obligation names the exact address that malloc returned.
malloc :: Pledge IO (RE Term) Addr
malloc = Pledge $ do
    addr <- randomRIO (1, 1000)
    return ( addr
           , universe                                      -- pre:    always allowed
           , Single (Atom "malloc" (List [Num addr]))      -- post:   emits malloc(addr)
           , finally (Atom "free"  (List [Num addr])) )    -- future: must free(addr) later
```

### Lifting plain actions

```haskell
liftPledge :: (Composable eff, Applicative m) => m a -> Pledge m eff a
```

Embeds any `m a` with trivial specifications:
`pre = universe`, `post = empty`, `fut = universe`.

### Running / inspecting

```haskell
data PledgeResult eff a = PledgeResult
    { ret  :: a
    , pre  :: eff
    , post :: eff
    , fut  :: eff
    }

inspect :: Functor m => Pledge m eff a -> m (PledgeResult eff a)
```

Prefer `inspect` over the individual accessors `getRet`/`getPre`/`getPost`/`getFut`
when `m` has observable side effects, since each accessor re-runs the action.

---

## The `Composable` Class

All `eff` types share a six-operation algebra:

```haskell
class Composable a where
    concatenation :: a -> a -> a   -- (·)   sequential composition
    conjunction   :: a -> a -> a   -- (⊓)   simultaneous constraint
    leftQuotient  :: a -> a -> a   -- (\)   left-quotient
    rightQuotient :: a -> a -> a   -- (∕)   right-quotient
    empty         :: a             -- identity for (·)
    universe      :: a             -- identity for (⊓)
```

| Operator | Fixity | Meaning |
|---|---|---|
| `(·)` | `infixl 6` | concatenation |
| `(⊓)` | `infixl 7` | conjunction |
| `(∖)` | `infixl 5` | left-quotient: `L ∖ R` = residual of `R` with `L` stripped from the left |
| `(∕)` | `infixl 5` | right-quotient: `L ∕ R` = residual of `L` with `R` stripped from the right |

The `Composable` instance lifts through any `Applicative`:

```haskell
instance (Composable eff, Applicative m) => Composable (m eff)
```

so `(·)`, `(⊓)`, `(∖)`, `(∕)` work directly on `m eff` values.

### Bind propagation laws

When `p >>= f` is evaluated, the monad propagates all four components:

```
pre  (p >>= f)  =  pre p  ⊓  (pre (f _)  ∕  post p)
post (p >>= f)  =  post p  ·   post (f _)
fut  (p >>= f)  =  (fut p  ∖  post (f _))  ⊓  fut (f _)
```

`pre (f _) ∕ post p` is the **right-quotient** — the residual precondition of `f`
not already discharged by `p`'s output (stripped from the right). When `post p`
fully covers `pre (f _)` the residual collapses to `empty` (obligation met); when
it does not, the residual is `Bot` / `∅` (violation detected).

`fut p ∖ post (f _)` is the **left-quotient** — the future obligation of `p`
remaining after `f`'s output has discharged what it can from the left.

The monad laws hold provided `Composable` satisfies eight algebraic laws for `∖`
(C1–C8) and four for `∕` (D1–D4); these are proved in `Pledge.Core`.

---

## Events

```haskell
data Term  = Str String | Num Int | List [Term]

data Event t = Atom String t   -- named event with a typed payload, e.g. send(42)
             | Wildcard         -- pattern that matches any single event (used in RE)
```

`subsumesEvent :: Eq t => Event t -> Event t -> Bool`
— `Wildcard` matches everything; a concrete `Atom` matches only itself.

The concrete term type used throughout the library is `Term`:
`Atom "free" (List [Num addr])`.

---

## `eff` Instances

### `RE t` — Regular Expressions

```haskell
data RE t
    = Bot              -- ∅         empty language
    | Epsilon          -- ε         empty word
    | Single (Event t) -- {e}       single-event language
    | Seq  (RE t) (RE t)    -- r₁ · r₂
    | Or   (RE t) (RE t)    -- r₁ ∪ r₂
    | And  (RE t) (RE t)    -- r₁ ∩ r₂
    | Star (RE t)           -- r*
    | Not  (RE t)           -- ¬r     complement (closed-form, no DFA needed)
```

Complement is handled algebraically via the Brzozowski derivative law
`∂ₐ(¬r) = ¬(∂ₐ(r))` and De Morgan rewriting during normalization.

#### Smart constructors

```haskell
top      :: RE t                     -- Σ*          = Not Bot
finally  :: Event t -> RE t          -- ◇e          = Σ* · e · Σ*
never    :: Event t -> RE t          -- ¬◇e         = Not (finally e)
globally :: Event t -> RE t          -- □e          = e*
noUntil  :: Event t -> Event t -> RE t  -- e must not occur before g
previously :: Event t -> RE t        -- alias for finally; use in pre-slots for clarity
```

#### LTL-to-RE translation

```haskell
data LTL t
    = LTLTrue | LTLFalse | LTLAtom (Event t)
    | LTLNot (LTL t) | LTLAnd (LTL t) (LTL t) | LTLOr (LTL t) (LTL t)
    | LTLNext     (LTL t)       -- X φ
    | LTLUntil    (LTL t) (LTL t)  -- φ U ψ
    | LTLFinally  (LTL t)       -- F φ
    | LTLGlobally (LTL t)       -- G φ

ltlToRe :: LTL t -> Maybe (RE t)
```

Returns `Nothing` when the Until left-hand side contains a temporal operator
with no single-step projection. No automaton construction is required.

#### `Composable (RE t)` summary

| Operation | RE semantics |
|---|---|
| `concatenation` | `Seq` |
| `conjunction` | `And` (intersection) |
| `leftQuotient r1 r2` | Antimirov quotient: residual of `r2` after `r1` (strip `r1` from left) |
| `rightQuotient r1 r2` | Reverse-Antimirov quotient: residual of `r2` before `r1` (strip `r1` from right) |
| `empty` | `Epsilon` |
| `universe` | `Not Bot` (= Σ*) |

---

### `SL` — Separation Logic

Symbolic separation-logic predicates over integer-addressed heaps.

```haskell
type Heap = Map Addr Val

data SL
    = Emp              -- empty heap               (identity for SepStar)
    | Top              -- any heap                 (identity for Conj)
    | Pure PPred       -- pure arithmetic predicate
    | Cell Addr Val    -- singleton: address a holds value v
    | SepStar SL SL    -- P * Q   separating conjunction
    | Conj   SL SL     -- P ⊓ Q   ordinary conjunction
    | Wand   SL SL     -- P -* Q  magic wand (residual)
```

| Composable operation | SL semantics |
|---|---|
| `concatenation` | `SepStar` (∗) |
| `conjunction` | `Conj` (⊓) |
| `leftQuotient p q` | magic wand `p -* q` |
| `rightQuotient p q` | magic wand `p -* q` (same as left-quotient; `SepStar` is commutative) |
| `empty` | `Emp` |
| `universe` | `Top` |

`normalizeSL` simplifies via `Emp`/`Top` identities and idempotency.

---

### `GuardedRE t` — Heap Invariants + Trace Ordering

A conjunction of a Presburger predicate and an RE, enforcing both heap and
trace constraints simultaneously.

```haskell
data GuardedRE a = GuardedRE PPred (RE a)
-- satisfies (heap, trace)  iff  heap |= PPred  ⊓  trace ∈ L(RE)
```

#### Construction

```haskell
fromRE    :: RE a    -> GuardedRE a   -- lift RE  (PPred = PTrue)
fromPPred :: PPred   -> GuardedRE a   -- lift PPred (RE = Σ*)
conjoin   :: Eq a => GuardedRE a -> GuardedRE a -> GuardedRE a
```

#### Membership

```haskell
nullableGuarded :: Map Addr Int -> GuardedRE a -> IO Bool
checkGuarded    :: Eq a => Map Addr Int -> [Event a] -> GuardedRE a -> IO Bool
```

`checkGuarded` folds `deriveGuarded` over the trace and then calls
`nullableGuarded`, which checks RE nullability and discharges the Presburger
constraint to Z3 via SBV.

---

### `WRE w t` — Weighted Regular Expressions

RE whose transitions carry weights from a semiring `w`. The language of a `WRE`
is a function `Σ* → w`.

```haskell
class (Eq w, Show w) => Semiring w where
    szero :: w;  sone :: w;  sadd :: w -> w -> w;  smul :: w -> w -> w

data WRE w t
    = WBot | WEps w | WSingle w (Event t)
    | WSeq (WRE w t) (WRE w t)
    | WAdd (WRE w t) (WRE w t)   -- ⊕ (choice)
    | WAnd (WRE w t) (WRE w t)   -- pointwise ⊗
    | WStar (WRE w t)

wNullable :: Semiring w => WRE w t -> w   -- weight of ε; generalises nullable :: RE -> Bool
```

#### Built-in semirings

| Type | `sadd` | `smul` | `szero` | `sone` | Use |
|---|---|---|---|---|---|
| `Bool` | `(||)` | `(&&)` | `False` | `True` | Recovers plain `RE` |
| `Prob` | `min(p+q, 1)` | `p*q` | `0` | `1` | Probability obligation is met |
| `Tropical` | `min` | `+` | `∞` | `0` | Minimum-cost path to discharge |

> **Note:** `Prob` uses saturating addition and is therefore only an
> *approximate* semiring. Use it for upper-bound probability reasoning only.

#### Smart constructors

```haskell
wTop      :: Semiring w => WRE w t
wFinally  :: Semiring w => w -> Event t -> WRE w t   -- Σ* · [w]e
wGlobally :: Semiring w => w -> Event t -> WRE w t   -- ([w]e)*
```

#### Example

```haskell
-- 95% probability that malloc is eventually freed.
malloc :: Pledge IO (WRE Prob Term) Addr
malloc = Pledge $ do
    addr <- randomRIO (1, 1000)
    return ( addr
           , wTop
           , WSingle sone (Atom "malloc" (List [Num addr]))
           , wFinally (Prob 0.95) (Atom "free" (List [Num addr])) )

-- wNullable (pledgeFut result) == Prob 0.95  when obligation not yet discharged.
```

---

## Presburger Arithmetic

Linear arithmetic over heap values, used in `SL` and `GuardedRE`.

```haskell
data PExpr
    = Lit Int           -- integer literal
    | ValAt Addr        -- h[a]: value at address a
    | Add PExpr PExpr
    | Mul Int   PExpr   -- scalar multiplication (preserves linearity)

data PPred
    = PTrue | PFalse
    | PLt PExpr PExpr | PLe PExpr PExpr | PEq PExpr PExpr
    | PGt PExpr PExpr | PGe PExpr PExpr
    | PNot PPred | PAnd PPred PPred
```

`normalizePPred` simplifies by eliminating `PTrue`/`PFalse`, deduplicating
conjuncts, substituting ground equalities, and evaluating literal comparisons.

`checkPPred :: PPred -> IO SolverResult` discharges satisfiability to Z3 via SBV.

---

## Checking a Program

```haskell
openFile :: String -> Pledge IO (RE Term) ()
openFile path = Pledge $ return
    ( ()
    , universe
    , Single (Atom "open"  (List [Str path]))
    , finally (Atom "close" (List [Str path])) )

closeFile :: String -> Pledge IO (RE Term) ()
closeFile path = Pledge $ return
    ( ()
    , previously (Atom "open" (List [Str path]))
    , Single (Atom "close" (List [Str path]))
    , universe )

program :: Pledge IO (RE Term) ()
program = openFile "data.txt" >> closeFile "data.txt"
```

```haskell
do PledgeResult _ pre _ fut <- inspect program
   print (normalize pre)   -- Σ*  ← precondition satisfied
   print (normalize fut)   -- Σ*  ← all future obligations discharged
```

`normalize pre == universe` means all preconditions are satisfied.
`normalize fut == universe` means no obligations remain.
Any other value is the outstanding (violated or unmet) condition.

---

## Module Layout

```
Pledge.hs                  -- re-exports every library module; import Pledge
Pledge/
  Core.hs                  -- Composable class, operators, Pledge monad, monad law proofs
  Event.hs                 -- Term, Event, subsumesEvent, Addr, Val
  Presburger.hs            -- PExpr, PPred, normalizePPred (re-exports Event)
  Presburger/Solver.hs     -- checkPPred via Z3/SBV
  RE.hs                    -- RE, normalize, derivative, LTL translation, QuickCheck laws
  SL.hs                    -- SL, normalizeSL, Composable instance
  GuardedRE.hs             -- GuardedRE, checkGuarded, Composable instance
  Semiring.hs              -- Semiring class, Prob, Tropical
  WeightedRE.hs            -- WRE, wNullable, wDerivative, Composable instance
Examples/
  RE/                      -- Memory, FileHandle, Mutex, Transaction, …
  SL/                      -- HeapMemory, BankAccount, LinkedList
  GuardedRE/               -- Memory (heap + trace), BoundedCounter
  WeightedRE/              -- Memory (Prob), TaskScheduler (Tropical)
  UnitTest/                -- QuickCheck RE laws, Presburger solver tests
Formalization/             -- Lean 4 mechanization of the core theory
```

### Module dependency graph

```
Pledge.Event
  └── Pledge.Presburger
        ├── Pledge.RE  ──────────────────────┐
        ├── Pledge.SL  ──────────────────────┤
        ├── Pledge.GuardedRE  ───────────────┤── Pledge.hs
        └── Pledge.Semiring                  │   (re-exports all)
              └── Pledge.WeightedRE  ────────┘
Pledge.Core  ──────────── (required by all above)
```

---

## Building and Running

```bash
# Type-check all modules
make check

# Run all examples
cabal run pledge-main

# GHCi
ghci -package-env=. Pledge.hs

# Presburger solver tests (requires Z3)
cabal run pledge-solver-test
```

Each example prints:

| Field | Meaning |
|---|---|
| `Pre` | `Σ*` = precondition satisfied; anything else = residual requirement |
| `Post` | the trace of events emitted |
| `Future` | `Σ*` = all obligations discharged; anything else = outstanding |
| `Weight` | (WRE only) semiring weight of the future at ε |

---

## Lean 4 Formalization

`Formalization/PledgeMonadLaws.lean` contains a mechanized proof of the
`Pledge` monad laws under the `Composable` axioms (C1–C8, D1–D4) in Lean 4.

```bash
cd Formalization && lake build Pledge
```

| Theorem | Status |
|---|---|
| Left identity (`pledge_left_id`) | proved |
| Right identity (`pledge_right_id`) | proved |
| Associativity (`pledge_assoc`) | proved |
