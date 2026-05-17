# Pledge

A Haskell library for effectful computations with **temporal specifications**: pre-conditions, post-conditions, and *future conditions* that enforce what must happen later in program execution.

## Motivation

Pre- and post-conditions reason about a single call boundary. Many real-world correctness properties are **deferred obligations** — things that must happen at some unknown future point:

- Every `malloc` must eventually be followed by a `free`
- Every `open` must eventually be followed by a `close`
- Every `beginTx` must eventually reach a `commit` or `rollback`
- Every `acquire` must eventually be followed by `release`

`Pledge` makes these obligations first-class values that compose automatically through monadic sequencing.

## Core Type

Every effectful operation carries three trace specifications:

```haskell
data Pledge eff a = Pledge
    { ret    :: a          -- return value
    , pre    :: eff        -- what must have happened immediately before
    , post   :: eff        -- what this operation produces
    , future :: a -> eff   -- obligation indexed by the return value
    }
```

`future` is **data-dependent**: it takes the operation's return value and produces the remaining obligation. This lets a single operation express obligations that name the exact resource handle returned:

```haskell
malloc addr = Pledge
    { ret    = addr
    , future = \a -> finally (Atom "free" (List [Num a]))
      --              ^ obligation names the address actually returned
    }
```

`eff` can be instantiated to any of:

| Type | Description |
|---|---|
| `RE` | Regular expressions over events — trace ordering obligations |
| `SL` | Separation-logic predicates — heap ownership |
| `GuardedRE` | Conjunction of a Presburger predicate and an RE — heap invariants + trace ordering |
| `WRE Prob` | Weighted RE over the probability semiring — reliability of obligations |
| `WRE Tropical` | Weighted RE over the tropical semiring — minimum cost to discharge obligations |

A convenience accessor evaluates `future` at the computation's own return value:

```haskell
evalFuture :: Pledge eff a -> eff
evalFuture e = future e (ret e)
```

## Bind Laws

When operations are sequenced via `>>=`, the `Pledge` monad propagates specifications automatically.

### Precondition (Hoare-logic style)

```
pre(e >>= f)  =  pre e /\ (post e \\ pre fe)
```

`post(e) \\ pre(f)` is the **Brzozowski quotient** — the residual precondition of `f` not already discharged by `e`'s postcondition. When `post(e)` fully covers `pre(f)`, the quotient collapses to `ε` and the overall `pre` is just `pre(e)`. When `post(e)` does not cover `pre(f)`, the residual is `∅` — flagging the violation.

### Future condition

```
future(e >>= f)  =  \_ -> (post fe \\ future e (ret e)) /\ future fe (ret fe)
```

- `\\` removes from `future(e)` the obligations discharged by `f`'s `post`
- `/\` intersects the remaining obligation with `f`'s own future condition

When `future` normalises to `universe` (Σ*, the universal language), all obligations are discharged.

## Events and the RE Language

Events are named occurrences with a typed argument:

```haskell
data Term  = Var String | Str String | Num Int | List [Term]
data Event = Atom String Term   -- e.g. Atom "send" (Num 42)
           | Wildcard           -- matches any single event (used in RE patterns)
```

The `RE` type is an **extended regular expression** with complement as a first-class operator:

| Constructor | Meaning |
|---|---|
| `Bot` | empty language ∅ |
| `Epsilon` | empty word ε |
| `Single e` | exactly event `e` (`Wildcard` matches any event) |
| `Seq r1 r2` | `r1` followed by `r2` |
| `Or r1 r2` | union `r1 ∨ r2` |
| `And r1 r2` | intersection `r1 ∧ r2` |
| `Star r` | Kleene star `r*` |
| `Not r` | complement `¬r` |

Complement is handled **algebraically** — no automaton construction:

```
∂_a(¬r)  =  ¬(∂_a(r))          -- derivative commutes with complement
 ν(¬r)   =  ¬ν(r)               -- nullability inverts under complement
```

The normaliser applies De Morgan laws (`¬(r₁ ∨ r₂) = ¬r₁ ∧ ¬r₂`) and double-negation elimination (`¬¬r = r`) during simplification.

Key derived patterns:

```haskell
universe :: RE               -- Σ* = ¬∅  (universal language)
universe = Not Bot

finally :: Event -> RE       -- eventually e  =  Σ* · e · Σ*
finally e = Seq universe (Seq (Single e) universe)

never :: Event -> RE         -- never e  =  ¬(Σ* · e · Σ*)
never e = Not (finally e)

noUntil :: Event -> Event -> RE  -- e must not occur before g
noUntil e g = Not (Seq (Star (And (Single Wildcard) (Not (Single g))))
                       (Seq (Single e) universe))
```

## LTL to RE Translation

LTL formulae over finite traces translate algebraically to `RE` — no automaton needed:

```haskell
data LTL
    = LTLTrue | LTLFalse
    | LTLAtom  Event
    | LTLNot   LTL  |  LTLAnd LTL LTL  |  LTLOr LTL LTL
    | LTLNext     LTL          -- X φ
    | LTLUntil    LTL LTL      -- φ U ψ
    | LTLFinally  LTL          -- F φ  ≜  Σ* · ⟦φ⟧
    | LTLGlobally LTL          -- G φ  ≜  ¬(Σ* · ¬⟦φ⟧)
```

```haskell
ltl_to_re (LTLNot l)        = Not  (ltl_to_re l)
ltl_to_re (LTLNext l)       = Seq  (Single Wildcard) (ltl_to_re l)
ltl_to_re (LTLFinally l)    = Seq  universe (ltl_to_re l)
ltl_to_re (LTLGlobally l)   = Not  (Seq universe (Not (ltl_to_re l)))
ltl_to_re (LTLUntil l1 l2)  = Seq  (Star (toSingleStep l1)) (ltl_to_re l2)
```

## Presburger Arithmetic Predicates

`PPred` is a language of **linear arithmetic predicates** over heap values, used in both `SL` and `GuardedRE`:

```haskell
data PExpr = Lit Int | ValAt Addr | Add PExpr PExpr | Mul Int PExpr
data PPred
    = PTrue | PFalse
    | PLt PExpr PExpr | PLe PExpr PExpr | PEq PExpr PExpr
    | PGt PExpr PExpr | PGe PExpr PExpr
    | PNot PPred | PAnd PPred PPred
```

`normalizePPred` simplifies predicates by:
- Eliminating `PTrue` from `PAnd` (identity) and absorbing with `PFalse`
- Deduplicating conjuncts
- Substituting ground equalities (`h[a] = k`) into sibling conjuncts
- Evaluating literal comparisons (`3 < 5 → PTrue`, `0 > 0 → PFalse`)

`checkPPred :: PPred -> IO SolverResult` discharges satisfiability queries to Z3 via SBV.

## Separation Logic (SL)

`Pledge` can instantiate `eff` to `SL` — symbolic separation-logic predicates — for heap-reasoning:

```haskell
data SL
    = Emp              -- empty heap (identity for SepStar)
    | Top              -- any heap   (identity for Conj)
    | Pure PPred       -- pure arithmetic predicate over heap values
    | Cell Addr Val    -- singleton: address owns value
    | SepStar SL SL    -- P * Q   separating conjunction
    | Conj   SL SL     -- P ∧ Q   ordinary conjunction
    | Wand   SL SL     -- P -* Q  magic wand (residual / subtraction)
```

The `Composable SL` instance uses `SepStar` as concatenation, `Conj` as conjunction, and the magic wand `P -* Q` as the subtraction operator (replacing the Brzozowski quotient).

## GuardedRE

`GuardedRE` pairs a Presburger predicate with an RE as a **conjunction of two independent constraints**:

```haskell
data GuardedRE = GuardedRE PPred RE
```

A state `(heap, trace)` satisfies `GuardedRE p r` iff `heap |= p` (Presburger side) **and** `trace ∈ L(r)` (trace side). This lets a single `eff` type enforce both heap invariants and event-ordering obligations simultaneously:

```haskell
-- free requires h[addr] > 0 (heap liveness) AND malloc was previously observed (trace)
free addr = Pledge
    { pre    = GuardedRE (PGt (ValAt addr) (Lit 0))
                         (previously (Atom "malloc" (List [Num addr])))
    , ...
    }
```

Key operations:
- `conjoin :: GuardedRE -> GuardedRE -> GuardedRE` — `(p1,r1) ∧ (p2,r2) = (p1∧p2, r1∩r2)`
- `deriveGuarded :: Event -> GuardedRE -> GuardedRE` — advances only the RE side; `PPred` is static
- `nullableGuarded :: Map Addr Int -> GuardedRE -> IO Bool` — checks both nullable RE and SAT predicate
- `checkGuarded :: Map Addr Int -> [Event] -> GuardedRE -> IO Bool` — full membership test

Smart constructors:
```haskell
fromRE    :: RE    -> GuardedRE   -- lift RE (no heap constraint)
fromPPred :: PPred -> GuardedRE   -- lift PPred (any trace)
```

## WeightedRE

`WRE w` is a regular expression whose transitions carry **weights from a semiring `w`**, generalising the Boolean `RE`:

```haskell
data WRE w
    = WBot               -- zero (empty language)
    | WEps w             -- ε accepted with weight w
    | WSingle w Event    -- single event accepted with weight w
    | WSeq (WRE w) (WRE w)
    | WAdd (WRE w) (WRE w)   -- weighted choice  (⊕)
    | WAnd (WRE w) (WRE w)   -- weighted conjunction
    | WStar (WRE w)
```

The language of `WRE w` is a function `Σ* → w`. `wNullable r` returns the semiring weight of ε, generalising `nullable :: RE -> Bool`.

### Semiring instances

```haskell
class (Eq w, Show w) => Semiring w where
    szero :: w;  sone :: w;  sadd :: w -> w -> w;  smul :: w -> w -> w
```

| Instance | `sadd` | `smul` | `szero` | `sone` | Use case |
|---|---|---|---|---|---|
| `Bool` | `\|\|` | `&&` | `False` | `True` | Recovers plain `RE` |
| `Prob` | `min(+,1)` | `×` | `0` | `1` | Probability an obligation is met |
| `Tropical` | `min` | `+` | `∞` | `0` | Minimum steps to discharge |

`instance Semiring w => Composable (WRE w)` makes `Pledge (WRE Prob) a` and `Pledge (WRE Tropical) a` work out of the box:

```haskell
-- Probability: end-to-end reliability of malloc → free
malloc addr = Pledge
    { future = \a -> wFinally (Prob 0.95) (Atom "free" (List [Num a])) }

-- wNullable (evalFuture mallocThenFree) = 95%
```

```haskell
-- Tropical: minimum steps to complete a submitted task
submit taskId = Pledge
    { future = \_ -> wFinally (Tropical 1) (Atom "complete" (List [Num taskId])) }

-- wNullable (evalFuture submitOnly) = ∞ steps  (obligation not dischargeable)
```

## The Composable Class

All `eff` types are instances of a shared algebra:

```haskell
class Composable a where
    concatenation :: a -> a -> a   -- (<>)  Seq / SepStar / WSeq
    conjunction   :: a -> a -> a   -- (/\)  And / Conj  / WAnd
    subtraction   :: a -> a -> a   -- (\\)  Brzozowski quotient / magic wand
    empty         :: a             -- ε   / Emp  / WEps sone
    universe      :: a             -- Σ*  / Top  / WStar (WSingle sone Wildcard)
```

| Operator | Precedence | Meaning |
|---|---|---|
| `<>` | 6 (left) | concatenation |
| `/\` | 7 (left) | conjunction |
| `\\` | 5 (left) | subtraction (quotient / wand) |

## Shadow: Spec alongside a Real Effect Handler

`Shadow.hs` demonstrates running `Pledge RE a` as a **pure specification** alongside a real `effectful` computation. The two monads stay completely separate — Pledge checks the spec statically; the handler library runs the actual effects.

```haskell
data Shadow a = Shadow
    { spec :: Pledge RE a                  -- checked statically
    , impl :: Eff '[FileSystem, IOE] a     -- run by the handler
    }

shOpen path = Shadow (specOpen path) (fsOpen path)
```

## Defining Your Own Operations

```haskell
openFile :: String -> Pledge RE ()
openFile path = Pledge
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "open" (List [Str path]))
    , future = \_ -> finally (Atom "close" (List [Str path]))
    }

closeFile :: String -> Pledge RE ()
closeFile path = Pledge
    { ret    = ()
    , pre    = Or (Single (Atom "open" (List [Str path])))
                  (Single (Atom "read" (List [Str path])))
    , post   = Single (Atom "close" (List [Str path]))
    , future = \_ -> universe
    }
```

```haskell
program :: Pledge RE ()
program = do
    openFile "data.txt"
    closeFile "data.txt"

-- normalize (pre         program) == universe  =>  all preconditions satisfied
-- normalize (evalFuture  program) == universe  =>  all future obligations met
```

## File Layout

```
./
├── Pledge.hs              -- top-level re-export: all library modules
├── Makefile               -- make check / make clean
├── Pledge/                -- library source
│   ├── Core.hs                -- Composable class + operators, Pledge monad
│   ├── Presburger.hs          -- Term, Event, PExpr, PPred, normalizePPred, checkPPred
│   ├── RE.hs                  -- RE, derivatives, normalize, LTL translation
│   ├── SL.hs                  -- SL, separating conjunction, magic wand
│   ├── GuardedRE.hs           -- GuardedRE = (PPred, RE), Composable instance
│   ├── Semiring.hs            -- Semiring class, Prob, Tropical instances
│   └── WeightedRE.hs          -- WRE w, wNullable, wDerivative, Composable instance
└── Examples/
    ├── Main.hs                -- runs all examples
    ├── UnitTest/
    │   ├── PledgeTest.hs          -- RE / monad unit tests
    │   └── PresburgerTest.hs      -- checkPPred solver tests
    ├── RE/                    -- RE instance examples
    │   ├── Memory.hs              -- malloc / free (data-dependent future)
    │   ├── FileHandle.hs          -- open / read / close
    │   ├── Mutex.hs               -- acquire / release
    │   ├── Transaction.hs         -- beginTx / commit / rollback
    │   ├── CryptoSession.hs       -- initSession / nonce lifecycle
    │   ├── NetworkProtocol.hs     -- TCP-like three-way handshake
    │   ├── Capability.hs          -- token / privilege lifecycle
    │   ├── Sensor.hs              -- IoT sensor / motor control
    │   └── Shadow.hs              -- Pledge spec alongside effectful handler
    ├── SL/                    -- separation-logic instance examples
    │   ├── HeapMemory.hs          -- alloc / free / read / write
    │   ├── BankAccount.hs         -- deposit / withdraw / transfer
    │   └── LinkedList.hs          -- node alloc / unlink / ownership
    ├── GuardedRE/             -- GuardedRE instance examples
    │   ├── Memory.hs              -- heap liveness (PPred) + trace ordering (RE)
    │   └── BoundedCounter.hs      -- arithmetic bounds + inc/dec/snapshot protocol
    └── WRE/                   -- WeightedRE instance examples
        ├── Memory.hs              -- probabilistic malloc/free (Prob semiring)
        └── TaskScheduler.hs       -- min-cost task scheduling (Tropical semiring)
```

### Module Dependency Graph

```
Pledge.Core ────────────────────────────────────────────┐
                                                        │
Pledge.Presburger ──┬──► Pledge.RE ────────────────────►│
                    │                                   │
                    ├──► Pledge.SL ────────────────────►│
                    │                                   ▼
                    ├──► Pledge.GuardedRE              Pledge.hs
                    │       (RE + Presburger)           (re-exports all)
                    │                                   ▲
                    └──► Pledge.Semiring ──► Pledge.WeightedRE ──►│
```

`Pledge.hs` re-exports all library modules as a single import surface — just `import Pledge`.

## Running the Examples

Type-check all modules at once:

```bash
make check
```

Or load interactively in GHCi:

```bash
ghci
:l Examples/Main.hs
```

Or run directly with `runghc`:

```bash
runghc -i. Examples/Main.hs
```

Each example prints per test program:

| Field | Meaning |
|---|---|
| `Pre` | `Σ*` = preconditions satisfied; `∅` = violated |
| `Post` | trace of events produced |
| `Future` | `Σ*` = obligations discharged; otherwise = outstanding obligation |
| `Weight` | (WRE only) semiring weight of the future at ε |

## Lean 4 Formalization

The `Formalization/` directory contains a Lean 4 mechanization of the core theory, covering syntax, denotational semantics, nullability, Brzozowski derivatives, normalization soundness, the `Composable` algebra, and the `Pledge` monad laws.

```bash
cd Formalization
lake build Pledge
```

### Proof Status

| Section | Proved | With `sorry` |
|---|---|---|
| Syntax & semantics (`inL`, `InStar`, `langEquiv`) | ✓ | — |
| Nullability (`nullable_iff`) | ✓ | — |
| Derivative correctness (all non-star cases + `not`) | ✓ | — |
| Derivative correctness (star/nil subcase) | — | ✓ |
| Normalization soundness (seq-ε, not, star cases) | ✓ | — |
| Normalization soundness (or/and cases) | — | ✓ |
| Algebraic language laws (De Morgan, distributivity) | ✓ | — |
| Monad laws: left/right identity | ✓ | — |
| Monad laws: associativity of `post` | ✓ | — |
| Monad laws: associativity of `future` | — | ✓ |
| Hoare-rule precondition collapse/violation | ✓ | — |
| Future-condition propagation correctness | ✓ | — |
| Temporal correctness (`no_leak_iff`) | ✓ | — |
