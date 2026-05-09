# FutureCond

A Haskell library for effectful computations with **temporal specifications**: pre-conditions, post-conditions, and *future conditions* that enforce what must happen later in program execution.

## Motivation

Pre- and post-conditions reason about a single call boundary. Many real-world correctness properties are **deferred obligations** — things that must happen at some unknown future point:

- Every `malloc` must eventually be followed by a `free`
- Every `open` must eventually be followed by a `close`
- Every `beginTx` must eventually reach a `commit` or `rollback`
- Every `acquire` must eventually be followed by `release`

`FutureCond` makes these obligations first-class values that compose automatically through monadic sequencing.

## Core Idea

Every effectful operation carries three trace specifications:

```haskell
data Effectful eff a = Effectful
    { ret    :: a    -- return value
    , pre    :: eff  -- what must have happened immediately before
    , post   :: eff  -- what this operation produces
    , future :: eff  -- what the rest of the program must eventually do
    }
```

`eff` is instantiated to `RE` — a regular expression over events, extended with complement.

### Precondition propagation (Hoare-logic style)

When operations are sequenced via `>>=`, the composed precondition is:

```
pre(e >>= f)  =  pre(e)  <>  (pre(f(ret e)) \\ post(e))
```

The term `pre(f) \\ post(e)` is the **residual** precondition of `f` not discharged by `e`'s postcondition.
When `post(e)` fully satisfies `pre(f)`, the quotient collapses to `ε` and the overall precondition is just `pre(e)`.
When `post(e)` does not satisfy `pre(f)`, the residual is `∅` and the composed `pre` becomes `∅` — flagging the violation.

### Future condition propagation

```
future(e >>= f)  =  (post(f(ret e)) \\ future(e))  /\  future(f(ret e))
```

- `\\` (subtraction / Brzozowski quotient) removes from `future(e)` the obligations discharged by `f`'s `post`
- `/\` (conjunction) intersects the remaining obligation with `f`'s own future condition

When `future` normalises to `¬∅` (the universal language), all obligations are discharged.

## Events and the RE Language

Events are named occurrences with typed arguments:

```haskell
data Term  = Var String | Str String | Num Int
data Event = Atom String [Term]   -- e.g.  Atom "send" [Num 42]
           | Wildcard             -- matches any single event (used in RE patterns)
```

The `RE` type is an **extended regular expression** — regular expressions with complement as a first-class operator:

| Constructor | Meaning |
|---|---|
| `Bot` | empty language ∅ |
| `Epsilon` | empty word ε |
| `Single e` | exactly the event `e` (with `Wildcard` matching any event) |
| `Seq r1 r2` | `r1` followed by `r2` |
| `Or r1 r2` | union `r1 ∨ r2` |
| `And r1 r2` | intersection `r1 ∧ r2` |
| `Star r` | Kleene star `r*` |
| `Not r` | complement `¬r` |

Complement is handled **algebraically** via two laws, without automaton construction:

```
∂_a(¬r)  =  ¬(∂_a(r))          -- derivative commutes with complement
ν(¬r)    =  ¬ν(r)               -- nullability inverts under complement
```

The normaliser applies De Morgan laws (`¬(r₁ ∨ r₂) = ¬r₁ ∧ ¬r₂`) and double-negation elimination (`¬¬r = r`) during simplification.

Two key derived values:

```haskell
anything :: RE          -- Σ* = ¬∅  (universal language; obligation discharged)
anything = Not Bot

finally :: Event -> RE  -- eventually e  =  Σ* · e · Σ*
finally e = Seq anything (Seq (Single e) anything)

globally :: Event -> RE -- always e  =  e*
globally e = Star (Single e)
```

## LTL to RE Translation

LTL formulae over finite traces can be translated algebraically to `RE`:

```haskell
data LTL
    = LTLTrue | LTLFalse
    | LTLAtom  Event
    | LTLNot   LTL
    | LTLAnd   LTL LTL  |  LTLOr  LTL LTL
    | LTLNext  LTL                       -- X φ
    | LTLUntil LTL LTL                  -- φ U ψ
    | LTLFinally  LTL                   -- F φ  ≜  Σ* · ⟦φ⟧
    | LTLGlobally LTL                   -- G φ  ≜  ¬(Σ* · ¬⟦φ⟧)
```

```haskell
ltl_to_re (LTLNot l)        = Not (ltl_to_re l)               -- ¬⟦l⟧
ltl_to_re (LTLNext l)       = Seq (Single Wildcard) (ltl_to_re l)  -- Σ · ⟦l⟧
ltl_to_re (LTLFinally l)    = Seq anything (ltl_to_re l)      -- Σ* · ⟦l⟧
ltl_to_re (LTLGlobally l)   = Not (Seq anything (Not (ltl_to_re l))) -- ¬(Σ*·¬⟦l⟧)
```

No automaton construction is required; `Not` carries the complement through the RE algebra.

## Operators

| Operator | Precedence | Meaning |
|---|---|---|
| `<>` | 6 (left) | concatenation (`Seq`) |
| `/\` | 7 (left) | conjunction (`And`) |
| `\\` | 5 (left) | subtraction (Brzozowski quotient) |

## File Layout

```
FutureCond/
├── Future.hs              -- RE, Composable, Effectful monad, LTL
└── Examples/
    ├── Main.hs            -- runs all examples
    ├── Memory.hs          -- malloc / free
    ├── FileHandle.hs      -- open / read / close  (precondition chain)
    ├── Mutex.hs           -- acquire / release
    ├── Transaction.hs     -- beginTx / commit / rollback
    ├── CryptoSession.hs   -- initSession / nonce lifecycle
    ├── NetworkProtocol.hs -- TCP-like three-way handshake
    ├── Capability.hs      -- token / privilege lifecycle
    └── Sensor.hs          -- IoT sensor / motor control
```

## Defining Your Own Operations

```haskell
openFile :: String -> Effectful RE ()
openFile path = Effectful
    { ret    = ()
    , pre    = universe                              -- no precondition
    , post   = Single (Atom "open" [Str path])
    , future = finally (Atom "close" [Str path])   -- close must occur eventually
    }

-- Precondition: the last event must have been open or read
closeFile :: String -> Effectful RE ()
closeFile path = Effectful
    { ret    = ()
    , pre    = Or (Single (Atom "open" [Str path]))
                  (Single (Atom "read" [Str path]))
    , post   = Single (Atom "close" [Str path])
    , future = universe                              -- obligation discharged
    }
```

Sequence them in the `Effectful` monad:

```haskell
program :: Effectful RE ()
program = do
    openFile "data.txt"
    closeFile "data.txt"

-- normalize (pre    program) == universe  =>  all preconditions satisfied
-- normalize (future program) == anything  =>  all future obligations met
```

## Lean 4 Formalization

The `Formalization/` directory contains a Lean 4 mechanization of the core
FutureCond theory, covering syntax, denotational semantics, nullability,
Brzozowski derivatives, normalization soundness, the `Composable` algebra,
and the `Effectful` monad laws.

### Prerequisites

| Tool | Version | Install |
|---|---|---|
| **Lean 4** (via `elan`) | ≥ 4.14.0 | `curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf \| sh` |
| **Lake** (bundled with Lean 4) | — | included with `elan` |
| **Git** | any | for fetching Mathlib |

Verify your installation:

```bash
lean --version   # should print Lean 4.x.x
lake --version
```

### Build

The formalization has **no external dependencies** — it uses only Lean 4's
built-in `Init` library.

```bash
cd FutureCond/Formalization

# One step: just build
lake build FutureCond
```

No `lake update`, no `lake exe cache get`, no internet connection required.

A successful build produces no errors. Theorems marked `sorry` emit
a warning (`declaration uses sorry`) — these are documented work-in-progress
steps and do not block compilation.

### Interactive Exploration

Open `FutureCond.lean` in **VS Code** with the
[lean4 extension](https://marketplace.visualstudio.com/items?itemName=leanprover.lean4)
installed. The extension uses the project's `lakefile.lean` automatically:
no additional configuration is needed.

Hover over any theorem name to see its type and proof state.
Place the cursor inside a `by` block to inspect goals step by step.

Alternatively, use **Emacs** with
[`lean4-mode`](https://github.com/leanprover/lean4-mode).

### Checking a Single File

```bash
# Type-check only, without a full lake build
lean --project FutureCond.lean
```

### Project Structure

```
Formalization/
├── lakefile.lean          -- Lake project: declares mathlib dependency
├── FutureCond.lean        -- All formalization (single file)
└── .lake/                 -- Lake build cache (auto-generated)
    └── packages/
        └── mathlib/       -- Mathlib4 source (fetched by lake update)
```

### Dependency

The formalization depends on
[Mathlib4 v4.14.0](https://github.com/leanprover-community/mathlib4/tree/v4.14.0),
imported via:

```lean
import Mathlib.Data.List.Basic
import Mathlib.Logic.Basic
import Mathlib.Tactic
```

Only a small slice of Mathlib is used (list lemmas, basic logic, and
the `simp`/`tauto` tactic infrastructure).

### Proof Status

| Section | Fully proved | With `sorry` |
|---|---|---|
| Syntax & semantics (`inL`, `InStar`, `langEquiv`) | ✓ | — |
| Nullability (`nullable_iff`) | ✓ | — |
| Derivative correctness (all non-star cases + `not`) | ✓ | — |
| Derivative correctness (star/nil subcase) | — | ✓ |
| Normalization soundness (seq-ε, not, star cases) | ✓ | — |
| Normalization soundness (or/and cases) | — | ✓ |
| `InStar` normalization lemmas | ✓ | — |
| Algebraic language laws (De Morgan, distributivity, …) | ✓ | — |
| Monad laws: left/right identity | ✓ | — |
| Monad laws: associativity of `post` | ✓ | — |
| Monad laws: associativity of `future` | — | ✓ |
| Hoare-rule precondition collapse/violation | ✓ | — |
| Future-condition propagation correctness | ✓ | — |
| Temporal correctness (`no_leak_iff`) | ✓ | — |

## Running the Examples

From the `FutureCond/` directory:

```bash
runghc -i. Examples/Main.hs      -- all examples
runghc -i. Examples/Memory.hs    -- memory management only
runghc -i. Examples/Mutex.hs     -- mutex lifecycle only
```

Each example prints three fields for every test program:

| Field | Meaning |
|---|---|
| `Pre` | `universe` (¬∅) = all preconditions satisfied; `∅` = precondition violated |
| `Post` | trace of events produced by the computation |
| `Future` | `¬∅` = all obligations discharged; any other RE = outstanding obligation |

## Checking Results Programmatically

```haskell
preOk :: Effectful RE () -> Bool
preOk prog = normalize (pre prog) == universe

futureOk :: Effectful RE () -> Bool
futureOk prog = normalize (future prog) == anything
```

If `futureOk` returns `False`, inspect `normalize (future prog)` — the remaining `finally(...)` terms name the unmet obligations precisely.
