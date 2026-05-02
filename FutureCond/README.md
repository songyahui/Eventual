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

Every effectful operation is annotated with three fields:

```haskell
data Effectful eff a = Effectful
    { ret    :: a    -- return value
    , pre    :: eff  -- what must have happened before
    , post   :: eff  -- what this operation produces
    , future :: eff  -- what the rest of the program must eventually do
    }
```

`eff` is instantiated to `RE` — a regular expression over events. A `future` condition is a regular expression that the **remaining trace** of the program must match.

When two operations are sequenced via `>>=`, future conditions are composed as:

```
future(e >>= f)  =  (post(f(ret e)) \\ future(e))  /\  future(f(ret e))
```

- `\\` (subtraction) removes from `future(e)` the obligations that `f` will satisfy via its own `post`
- `/\` (conjunction) intersects the remaining obligations with `f`'s own future condition

The result: obligations accumulate and propagate compositionally. When `future` normalises to `anything` (`_*`), all obligations are discharged.

## Regular Expression Language

Events are `(String, [Term])` pairs. The `RE` type builds specifications over them:

| Constructor | Meaning |
|---|---|
| `Bot` | no trace satisfies this (∅) |
| `Epsilon` | empty trace (ε) |
| `Single e` | exactly the event `e` |
| `Wildcard` | any single event |
| `Seq r1 r2` | `r1` followed by `r2` |
| `Or r1 r2` | `r1` or `r2` |
| `And r1 r2` | traces satisfying both `r1` and `r2` |
| `Star r` | zero or more repetitions of `r` |

Two derived combinators are provided:

```haskell
anything :: FutureCond
anything = Star Wildcard          -- matches any trace (obligation discharged)

finally :: Event -> FutureCond
finally e = anything `Seq` Single e `Seq` anything  -- e must occur eventually
```

## Operators

| Operator | Precedence | Meaning |
|---|---|---|
| `<>` | 6 (left) | concatenation (`Seq`) |
| `/\` | 7 (left) | conjunction (`And`) |
| `\\` | 5 (left) | subtraction (Brzozowski-style derivative) |

## File Layout

```
FutureCond/
├── Future.hs          -- library: RE, Composable, Effectful monad
├── Main.hs            -- standalone demo (malloc/free)
└── Examples/
    ├── Main.hs        -- runs all examples
    ├── Memory.hs      -- malloc / free
    ├── FileHandle.hs  -- open / close
    ├── Mutex.hs       -- acquire / release
    ├── Transaction.hs -- beginTx / commit / rollback
    ├── CryptoSession.hs -- initSession / nonce lifecycle
    ├── NetworkProtocol.hs -- TCP-like handshake
    ├── Capability.hs  -- token / privilege lifecycle
    └── Sensor.hs      -- IoT sensor / motor control
```

## Running the Examples

From the `FutureCond/` directory:

```bash
# Run all examples
runghc Examples/Main.hs

# Run a single example
runghc Examples/Memory.hs
runghc Examples/Mutex.hs
```

Each example prints the normalised `future` condition of each test program. A result of `(_)*` means all temporal obligations are satisfied. Anything else identifies which obligation remains outstanding.

## Defining Your Own Operations

```haskell
-- An operation that must eventually be followed by its counterpart
openFile :: String -> Effectful RE ()
openFile path = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("open",  [Str path])
    , future = finally ("close", [Str path])
    }

closeFile :: String -> Effectful RE ()
closeFile path = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("close", [Str path])
    , future = anything                       -- obligation discharged
    }

program :: Effectful RE ()
program = do
    openFile "a.txt"
    openFile "b.txt"
    closeFile "a.txt"
    closeFile "b.txt"

-- normalize (future program) == anything  =>  all obligations met
```

## Checking Results

```haskell
check :: Effectful RE () -> Bool
check prog = normalize (future prog) == anything
```

If `check` returns `False`, inspect `normalize (future prog)` — the remaining `finally(...)` terms name the unmet obligations precisely.
