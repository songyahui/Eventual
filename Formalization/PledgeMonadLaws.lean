/-!
# PledgeMonadLaws

Formal verification that the `Pledge` monad from `Pledge/Core.hs` satisfies
the three monad laws under the `Composable` axioms (S1–S3, C1–C3, L1–L4, R1–R4).

## Model

`Pledge eff α` (pure / Id-monad form) bundles a return value with three
effect conditions:

```
  pure x    = (x,  ⊤,  ε,  ⊤)
  p >>= g   = let gp := g p.ret in
              (gp.ret,  p.pre ∧ (gp.pre ∕ p.post),  p.post ⋄ gp.post,
                        (p.future ∖ gp.post) ∧ gp.future)
```

## Notation (mirrors `Pledge/Core.hs`)

| Lean    | Haskell      | Meaning                                              |
|---------|--------------|------------------------------------------------------|
| `a ⋄ b` | `a · b`      | sequential concatenation                             |
| `a ⊓ b` | `a ⊓ b`      | conjunction / intersection                           |
| `r ∖ s` | `r ∖ s`      | left-quotient: residual of `r` after stripping prefix `s`  |
| `a ∕ b` | `a ∕ b`      | right-quotient: residual of `a` after stripping suffix `b` |
| `ε`     | `empty`      | unit for `⋄`                                        |
| `⊤`     | `universe`   | unit for `⊓`                                        |

`r ∖ s = leftQuotient s r` in Core.hs: dividend `r` on the left, divisor `s` on the right.
`a ∕ b = rightQuotient b a` in Core.hs: dividend `a` on the left, divisor `b` on the right.

## Axioms required

### Sequential algebra (`⋄`, S1–S3)

| Label | Statement                         | Used for            |
|-------|-----------------------------------|---------------------|
| S1    | `ε ⋄ a = a`                       | Law 1 post          |
| S2    | `a ⋄ ε = a`                       | Law 2 post          |
| S3    | `(a ⋄ b) ⋄ c = a ⋄ (b ⋄ c)`        | Law 3 post          |

### Meet/Conjunction algebra (`⊓`, C1–C3)

| Label | Statement                         | Used for                      |
|-------|-----------------------------------|-------------------------------|
| C1    | `a ∧ b = b ∧ a`                   | Law 2 (derives right identity)|
| C2    | `(a ∧ b) ∧ c = a ∧ (b ∧ c)`       | Law 3 pre, future             |
| C3    | `⊤ ∧ a = a`                       | Laws 1, 2                     |

### Left-quotient axioms (`∖`, L1–L4)

| Label | Statement                             | Used for       |
|-------|---------------------------------------|----------------|
| L1    | `a ∖ ε = a`                           | Law 2 future   |
| L2    | `⊤ ∖ r = ⊤`                           | Law 1 future   |
| L3    | `x ∖ (a ⋄ b) = (x ∖ a) ∖ b`           | Law 3 future   |
| L4    | `(a ∧ b) ∖ c = (a ∖ c) ∧ (b ∖ c)`     | Law 3 future   |

### Right-quotient axioms (`∕`, R1–R4)

| Label | Statement                             | Used for     |
|-------|---------------------------------------|--------------|
| R1    | `a ∕ ε = a`                           | Law 1 pre    |
| R2    | `⊤ ∕ q = ⊤`                           | Law 2 pre    |
| R3    | `(a ∕ b) ∕ c = a ∕ (c ⋄ b)`           | Law 3 pre    |
| R4    | `(a ∧ b) ∕ c = (a ∕ c) ∧ (b ∕ c)`     | Law 3 pre    |

The `Ccomm` axiom from the previous version is no longer required.
Law 3's `future` proof follows from L3 alone (the corrected left-quotient
sequential law `x ∖ (a⋄b) = (x∖a) ∖ b`) without needing commutativity of `⋄`.
-/

section PledgeMonad

-- ═══════════════════════════════════════════════════════════════════════════
-- § 1  Composable algebra
-- ═══════════════════════════════════════════════════════════════════════════

/-- Abstract algebra for effect types, matching `class Composable` in Core.hs.

  `lq r s`   = `leftQuotient s r`  = `r ∖ s`   in Haskell (dividend r, divisor s).
  `rqOp a b` = `rightQuotient b a` = `a ∕ b`   in Haskell (dividend a, divisor b). -/
class Composable (eff : Type) where
  cat   : eff → eff → eff   -- (·)   sequential composition
  meet  : eff → eff → eff   -- (∧)   conjunction
  lq    : eff → eff → eff   -- lq r s  =  leftQuotient s r  (dividend r, divisor s)
  rqOp  : eff → eff → eff   -- rqOp a b  =  rightQuotient b a  =  a ∕ b
  emp   : eff               -- empty:    unit for cat   (written ε)
  univ  : eff               -- universe: unit for meet  (written ⊤)

local infixl:70 " ⋄ "  => Composable.cat
local infixl:65 " ⊓ "  => Composable.meet  -- written ∧ in doc comments / Core.hs
local infixl:60 " ∖ "  => Composable.lq    -- r ∖ s  =  leftQuotient s r  (dividend r, divisor s)
local infixl:60 " ∕ "  => Composable.rqOp  -- a ∕ b  =  rightQuotient b a  (dividend a, divisor b)
local notation "ε"     => Composable.emp    -- identity for ⋄  (mirrors ε   in Core.hs)
local notation "⊤"     => Composable.univ   -- identity for ⊓  (mirrors (⊤) in Core.hs)

-- ═══════════════════════════════════════════════════════════════════════════
-- § 2  Axioms
-- ═══════════════════════════════════════════════════════════════════════════

/-- Algebraic laws required for the three monad-law proofs. -/
structure ComposableAxioms (eff : Type) [Composable eff] : Type where
  -- Sequential algebra (⋄)
  S1    : ∀ a : eff,         Composable.emp ⋄ a = a
  S2    : ∀ a : eff,         a ⋄ Composable.emp = a
  S3    : ∀ a b c : eff,     (a ⋄ b) ⋄ c = a ⋄ (b ⋄ c)
  -- Meet/Conjunction algebra (⊓)
  C1    : ∀ a b : eff,       a ⊓ b = b ⊓ a
  C2    : ∀ a b c : eff,     (a ⊓ b) ⊓ c = a ⊓ (b ⊓ c)
  C3    : ∀ a : eff,         Composable.univ ⊓ a = a
  -- Left-quotient laws (∖)
  L1    : ∀ a : eff,         a ∖ Composable.emp = a
  L2    : ∀ r : eff,         Composable.univ ∖ r = Composable.univ
  L3    : ∀ a b x : eff,     x ∖ (a ⋄ b) = (x ∖ a) ∖ b
  L4    : ∀ c a b : eff,     (a ⊓ b) ∖ c = (a ∖ c) ⊓ (b ∖ c)
  -- Right-quotient laws (∕)
  R1    : ∀ a : eff,         a ∕ Composable.emp = a
  R2    : ∀ q : eff,         Composable.univ ∕ q = Composable.univ
  R3    : ∀ a b x : eff,     (x ∕ b) ∕ a = x ∕ (a ⋄ b)
  R4    : ∀ c a b : eff,     (a ⊓ b) ∕ c = (a ∕ c) ⊓ (b ∕ c)

-- ═══════════════════════════════════════════════════════════════════════════
-- § 3  The Pledge structure
-- ═══════════════════════════════════════════════════════════════════════════

/-- Pure rendering of `newtype Pledge m eff a = Pledge { runPledge :: m (a,eff,eff,eff) }`. -/
structure Pledge (eff : Type) [Composable eff] (α : Type) : Type where
  ret    : α
  pre    : eff
  post   : eff
  future : eff

@[ext]
theorem Pledge.ext [Composable eff] {p q : Pledge eff α}
    (hr : p.ret = q.ret) (hp : p.pre = q.pre)
    (hq : p.post = q.post) (hf : p.future = q.future) : p = q := by
  cases p; cases q; simp_all

-- ═══════════════════════════════════════════════════════════════════════════
-- § 4  pure and bind
-- ═══════════════════════════════════════════════════════════════════════════

def Pledge.pure [Composable eff] (x : α) : Pledge eff α :=
  { ret    := x
    pre    := Composable.univ
    post   := Composable.emp
    future := Composable.univ }

def Pledge.bind [Composable eff] (p : Pledge eff α) (g : α → Pledge eff β) :
    Pledge eff β :=
  let gp := g p.ret
  { ret    := gp.ret
    pre    := p.pre ⊓ (gp.pre ∕ p.post)          -- ∧ (preB ∕ postA)   right-quotient
    post   := p.post ⋄ gp.post
    future := (p.future ∖ gp.post) ⊓ gp.future }  -- (futA ∖ postB) ∧ futB  left-quotient

-- ═══════════════════════════════════════════════════════════════════════════
-- § 5  Monad laws
-- ═══════════════════════════════════════════════════════════════════════════

variable {eff : Type} [Composable eff] (ax : ComposableAxioms eff)
variable {α β γ : Type}

/-!
### Law 1 — Left identity: `pure x >>= g = g x`

Let `g x = (b, P', Q', F')`.

| component | expression              | steps                           |
|-----------|-------------------------|---------------------------------|
| pre       | `⊤ ∧ (P' ∕ ε)`         | `= ⊤ ∧ P' = P'`   R1, C3       |
| post      | `ε ⋄ Q'`                | `= Q'`            S1             |
| future    | `(⊤ ∖ Q') ∧ F'`        | `= ⊤ ∧ F' = F'`   L2, C3       |
-/
theorem pledge_left_id (ax : ComposableAxioms eff) (x : α) (g : α → Pledge eff β) :
    Pledge.bind (Pledge.pure x) g = g x := by
  apply Pledge.ext
  · simp only [Pledge.bind, Pledge.pure]
  · simp only [Pledge.bind, Pledge.pure, ax.R1, ax.C3]
  · simp only [Pledge.bind, Pledge.pure, ax.S1]
  · simp only [Pledge.bind, Pledge.pure, ax.L2, ax.C3]

/-!
### Law 2 — Right identity: `p >>= pure = p`

Let `p = (a, P, Q, F)`.

| component | expression              | steps                                  |
|-----------|-------------------------|----------------------------------------|
| pre       | `P ∧ (⊤ ∕ Q)`          | `= P ∧ ⊤ = ⊤ ∧ P = P`   R2, C1, C3   |
| post      | `Q ⋄ ε`                | `= Q`                    S2            |
| future    | `(F ∖ ε) ∧ ⊤`          | `= F ∧ ⊤ = ⊤ ∧ F = F`   L1, C1, C3   |
-/
theorem pledge_right_id (ax : ComposableAxioms eff) (p : Pledge eff α) :
    Pledge.bind p Pledge.pure = p := by
  apply Pledge.ext
  · simp only [Pledge.bind, Pledge.pure]
  · simp only [Pledge.bind, Pledge.pure, ax.R2]; rw [ax.C1, ax.C3]
  · simp only [Pledge.bind, Pledge.pure, ax.S2]
  · simp only [Pledge.bind, Pledge.pure, ax.L1]; rw [ax.C1, ax.C3]

/-!
### Law 3 — Associativity: `(p >>= f) >>= g = p >>= (fun x => f x >>= g)`

Let `p=(a,P,Q,F)`, `f a=(b,P',Q',F')`, `g b=(c,P'',Q'',F'')`.

**LHS** (`(p >>= f) >>= g`):
```
  pre_L    = [P ∧ (P' ∕ Q)] ∧ (P'' ∕ (Q ⋄ Q'))
  post_L   = (Q ⋄ Q') ⋄ Q''
  future_L = ([(F ∖ Q') ∧ F'] ∖ Q'') ∧ F''
```

**RHS** (`p >>= (fun x => f x >>= g)`):
```
  pre_R    = P ∧ ([P' ∧ (P'' ∕ Q')] ∕ Q)
  post_R   = Q ⋄ (Q' ⋄ Q'')
  future_R = (F ∖ (Q' ⋄ Q'')) ∧ [(F' ∖ Q'') ∧ F'']
```

**post**: `post_L = post_R` by S3.

**pre** (expand RHS using R4, R3, then ←C2):
```
  [P' ∧ (P'' ∕ Q')] ∕ Q
    = (P' ∕ Q) ∧ ((P'' ∕ Q') ∕ Q)   by R4
    = (P' ∕ Q) ∧ (P'' ∕ (Q ⋄ Q'))   by R3
  P ∧ [(P' ∕ Q) ∧ (P'' ∕ (Q ⋄ Q'))]
    = [P ∧ (P' ∕ Q)] ∧ (P'' ∕ (Q ⋄ Q'))  by ←C2
```

**future** (expand LHS using L4, ←L3, then C2):
```
  [(F ∖ Q') ∧ F'] ∖ Q''
    = ((F ∖ Q') ∖ Q'') ∧ (F' ∖ Q'')   by L4
    = (F ∖ (Q' ⋄ Q'')) ∧ (F' ∖ Q'')   by ←L3  (x=F, a=Q', b=Q'')
  [(F ∖ (Q' ⋄ Q'')) ∧ (F' ∖ Q'')] ∧ F''
    = (F ∖ (Q' ⋄ Q'')) ∧ [(F' ∖ Q'') ∧ F'']   by C2
```
-/
theorem pledge_assoc
    (ax : ComposableAxioms eff)
    (p : Pledge eff α) (f : α → Pledge eff β) (g : β → Pledge eff γ) :
    Pledge.bind (Pledge.bind p f) g =
    Pledge.bind p (fun x => Pledge.bind (f x) g) := by
  apply Pledge.ext
  · -- ret: definitional
    simp only [Pledge.bind]
  · -- pre: [P∧(P'∕Q)]∧(P''∕(Q⋄Q')) = P∧([P'∧(P''∕Q')]∕Q)
    simp only [Pledge.bind]
    rw [ax.R4, ax.R3, ← ax.C2]
  · -- post: (Q⋄Q')⋄Q'' = Q⋄(Q'⋄Q'')
    simp only [Pledge.bind]
    rw [ax.S3]
  · -- future: ([(F∖Q')∧F']∖Q'')∧F'' = (F∖(Q'⋄Q''))∧[(F'∖Q'')∧F'']
    simp only [Pledge.bind]
    rw [ax.L4, ← ax.L3, ax.C2]

-- ═══════════════════════════════════════════════════════════════════════════
-- § 6  Summary
-- ═══════════════════════════════════════════════════════════════════════════

/-!
## Theorem inventory

| Theorem            | sorry-free? | Axioms consumed              |
|--------------------|-------------|------------------------------|
| `pledge_left_id`   | ✓           | S1, C3, L2, R1               |
| `pledge_right_id`  | ✓           | S2, C1, C3, L1, R2           |
| `pledge_assoc`     | ✓           | S3, C2, L3, L4, R3, R4       |

All three proofs are `sorry`-free.  `Ccomm` is not required: the corrected
L3 (`x ∖ (a⋄b) = (x∖a) ∖ b`) handles the `future` associativity step directly.

## Changes from previous version

- `res` renamed to `lq` (left-quotient); `rqOp` added (right-quotient).
- Notation convention: `r ∖ s` has dividend `r` on the left (matches Core.hs `a ∖ b = leftQuotient b a`).
- Bind `future` updated to `p.future ∖ gp.post` (dividend first); `pre` uses `∕` (right-quotient).
- L1–L4 restated in dividend-left form: `a ∖ emp`, `univ ∖ r`, `x ∖ (a⋄b) = (x∖a)∖b`, `(a∧b)∖c`.
- L3 corrected to `x ∖ (a⋄b) = (x∖a) ∖ b` (left-quotient sequential law).
- `Ccomm` axiom removed (was only needed to paper over the wrong L3 direction).
- R1–R4 axioms added for the right-quotient `∕`.
- Law 1/2 pre proofs now cite R1/R2 instead of L1/L2.
- Axioms regrouped: S1–S3 (sequential), C1–C3 (meet), L1–L4 (left-quotient), R1–R4 (right-quotient).
-/

end PledgeMonad
