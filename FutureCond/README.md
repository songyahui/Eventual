# FutureCond

A Haskell library for effectful computations with **temporal specifications**: pre-conditions, post-conditions, and *future conditions* that enforce what must happen later in program execution.

## Motivation

Pre- and post-conditions reason about a single call boundary. Many real-world correctness properties are **deferred obligations** — things that must happen at some unknown future point:

- Every `malloc` must eventually be followed by a `free`
- Every `open` must eventually be followed by a `close`
- Every `beginTx` must eventually reach a `commit` or `rollback`
- Every `acquire` must eventually be followed by `release`

`FutureCond` makes these obligations first-class values that compose automatically through monadic sequencing.

## Core Type

Every effectful operation carries three trace specifications:

```haskell
data FutureCond eff a = FutureCond
    { ret    :: a          -- return value
    , pre    :: eff        -- what must have happened immediately before
    , post   :: eff        -- what this operation produces
    , future :: a -> eff   -- obligation indexed by the return value
    }
```

`future` is **data-dependent**: it takes the operation's return value and produces the remaining obligation. This lets a single operation express obligations that name the exact resource handle returned:

```haskell
malloc addr = FutureCond
    { ret    = addr
    , future = \a -> finally (Atom "free" (List [Num a]))
      --              ^ obligation names the address actually returned
    }
```

`eff` is instantiated to `RE` (regular expressions over events) or `SL` (separation-logic predicates).

A convenience accessor evaluates `future` at the computation's own return value:

```haskell
evalFuture :: FutureCond eff a -> eff
evalFuture e = future e (ret e)
```

## Bind Laws

When operations are sequenced via `>>=`, the `FutureCond` monad propagates specifications automatically.

### Precondition (Hoare-logic style)

```
pre(e >>= f)  =  pre(e)  /\  (post(e) \\ pre(f(ret e)))
```

`post(e) \\ pre(f)` is the **Brzozowski quotient** — the residual precondition of `f` not already discharged by `e`'s postcondition. When `post(e)` fully covers `pre(f)`, the quotient collapses to `ε` and the overall `pre` is just `pre(e)`. When `post(e)` does not cover `pre(f)`, the residual is `∅` — flagging the violation.

### Future condition

```
future(e >>= f)  =  (post(f(ret e)) \\ future(e))  /\  future(f(ret e))
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

## Separation Logic (SL)

`FutureCond` can also instantiate `eff` to `SL` — symbolic separation-logic predicates — for heap-reasoning:

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

Example — heap cell alloc/free:

```haskell
alloc :: Addr -> Val -> FutureCond SL ()
alloc addr val = FutureCond
    { ret    = ()
    , pre    = Top                -- no ownership required
    , post   = Cell addr val      -- establishes ownership
    , future = \_ -> Top          -- no deferred obligation
    }

free :: Addr -> Val -> FutureCond SL ()
free addr val = FutureCond
    { ret    = ()
    , pre    = Cell addr val      -- must own the cell
    , post   = Emp                -- relinquishes ownership
    , future = \_ -> Top
    }
```

## The Composable Class

Both `RE` and `SL` are instances of a shared algebra:

```haskell
class Composable a where
    concatenation :: a -> a -> a   -- (<>)  Seq / SepStar
    conjunction   :: a -> a -> a   -- (/\)  And / Conj
    subtraction   :: a -> a -> a   -- (\\)  Brzozowski quotient / magic wand
    empty         :: a             -- ε   / Emp
    universe      :: a             -- Σ*  / Top
```

| Operator | Precedence | Meaning |
|---|---|---|
| `<>` | 6 (left) | concatenation |
| `/\` | 7 (left) | conjunction |
| `\\` | 5 (left) | subtraction (quotient / wand) |

## Shadow: Spec alongside a Real Effect Handler

`Shadow.hs` demonstrates running `FutureCond RE a` as a **pure specification** alongside a real `effectful` computation. The two monads stay completely separate — FutureCond checks the spec statically; the handler library runs the actual effects.

A `FileSystem` effect is declared as a GADT and interpreted by two handlers:

```haskell
data FileSystem :: Effect where
    FsOpen  :: FilePath -> FileSystem m ()
    FsRead  :: FilePath -> FileSystem m String
    FsClose :: FilePath -> FileSystem m ()

-- Handler: interprets FileSystem into IOE, logging events to an IORef
fsHandler :: IORef [String] -> EffectHandler FileSystem '[IOE]

-- Shadow: pairs the spec with the real Eff program
data Shadow a = Shadow
    { spec :: FutureCond RE a          -- checked statically
    , impl :: Eff '[FileSystem, IOE] a -- run by the handler
    }
```

Each `Shadow` operation bundles the spec primitive with the `effectful` send — same protocol, two interpretations:

```haskell
shOpen path = Shadow (specOpen path) (fsOpen path)
```

Programs are written in `Shadow` do-notation:

```haskell
goodFile :: Shadow String
goodFile = do
    shOpen "data.txt"
    contents <- shRead "data.txt"
    shClose "data.txt"
    return contents

-- spec:  Future = ε    (close obligation discharged)
-- trace: ["open(data.txt)", "read(data.txt)", "close(data.txt)"]
```

## Defining Your Own Operations

```haskell
openFile :: String -> FutureCond RE ()
openFile path = FutureCond
    { ret    = ()
    , pre    = universe                                   -- no precondition
    , post   = Single (Atom "open" (List [Str path]))
    , future = \_ -> finally (Atom "close" (List [Str path]))
    }

closeFile :: String -> FutureCond RE ()
closeFile path = FutureCond
    { ret    = ()
    , pre    = Or (Single (Atom "open" (List [Str path])))
                  (Single (Atom "read" (List [Str path])))
    , post   = Single (Atom "close" (List [Str path]))
    , future = \_ -> universe                             -- obligation discharged
    }
```

Sequence them in the `FutureCond` monad:

```haskell
program :: FutureCond RE ()
program = do
    openFile "data.txt"
    closeFile "data.txt"

-- normalize (pre         program) == universe  =>  all preconditions satisfied
-- normalize (evalFuture  program) == universe  =>  all future obligations met
```

## Checking Results Programmatically

```haskell
preOk :: FutureCond RE () -> Bool
preOk prog = normalize (pre prog) == universe

futureOk :: FutureCond RE () -> Bool
futureOk prog = normalize (evalFuture prog) == universe
```

If `futureOk` returns `False`, inspect `normalize (evalFuture prog)` — the remaining `F(...)` terms name the unmet obligations precisely.

## File Layout

```
FutureCond/
├── FutureCond.hs          -- RE, SL, Composable, FutureCond monad, LTL
├── futurecond.cabal       -- cabal project (deps: containers, effectful)
├── cabal.project
└── Examples/
    ├── Main.hs            -- runs all examples
    ├── UnitTest.hs        -- property tests
    ├── RE/                -- regular-expression examples
    │   ├── Memory.hs          -- malloc / free (data-dependent future)
    │   ├── FileHandle.hs      -- open / read / close
    │   ├── Mutex.hs           -- acquire / release
    │   ├── Transaction.hs     -- beginTx / commit / rollback
    │   ├── CryptoSession.hs   -- initSession / nonce lifecycle
    │   ├── NetworkProtocol.hs -- TCP-like three-way handshake
    │   ├── Capability.hs      -- token / privilege lifecycle
    │   ├── Sensor.hs          -- IoT sensor / motor control
    │   └── Shadow.hs          -- FutureCond spec + effectful handler
    └── SL/                -- separation-logic examples
        ├── HeapMemory.hs      -- alloc / free / read / write
        ├── BankAccount.hs     -- deposit / withdraw / transfer
        └── LinkedList.hs      -- node alloc / unlink / ownership
```

## Running the Examples

With cabal (recommended):

```bash
cd FutureCond
cabal build
cabal run futurecond-main
```

Or directly with `runghc`:

```bash
cd FutureCond
runghc -i. Examples/Main.hs
```

Each example prints three fields per test program:

| Field | Meaning |
|---|---|
| `Pre` | `Σ*` = all preconditions satisfied; `∅` = precondition violated |
| `Post` | trace of events produced by the computation |
| `Future` | `Σ*` = all obligations discharged; anything else = outstanding obligation |

## Lean 4 Formalization

The `Formalization/` directory contains a Lean 4 mechanization of the core theory, covering syntax, denotational semantics, nullability, Brzozowski derivatives, normalization soundness, the `Composable` algebra, and the `FutureCond` monad laws.

```bash
cd FutureCond/Formalization
lake build FutureCond
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
