/-!
# PledgeMonadLaws

Formal verification that the `Pledge` monad from `Pledge/Core.hs` satisfies
the three monad laws under the `Composable` axioms (C1–C8 + Ccomm).

## Model

`Pledge eff α` (pure / Id-monad form) bundles a return value with three
effect conditions:

```
  pure x    = (x,  univ,  emp,  univ)
  p >>= g   = let gp := g p.ret
              (gp.ret,  p.pre ⊓ (p.post ≺ gp.pre),  p.post ⋄ gp.post,
                        (gp.post ≺ p.future) ⊓ gp.future)
```

## Notation (mirrors `Pledge/Core.hs`)

| Lean   | Haskell      | Meaning                           |
|--------|--------------|-----------------------------------|
| `a ⋄ b`  | `a · b`    | sequential concatenation          |
| `a ⊓ b`  | `a /\\ b`   | conjunction / intersection        |
| `r ≺ s`  | `s \\\\ r`  | residual of `s` after `r`         |
| `emp`    | `empty`     | unit for `⋄`                      |
| `univ`   | `universe`  | unit for `⊓`                      |

`r ≺ s` = `subtraction r s` in the Haskell typeclass = `s \\ r` in Core.hs.

## Axioms required

| Label  | Statement                          | Used for              |
|--------|------------------------------------|-----------------------|
| C1     | `emp ⋄ a = a`                     | Law 1 post            |
| C2     | `a ⋄ emp = a`                     | Law 2 post            |
| C3     | `(a ⋄ b) ⋄ c = a ⋄ (b ⋄ c)`      | Law 3 post            |
| C4     | `univ ⊓ a = a`                    | Laws 1,2 pre,future   |
| C4r    | `a ⊓ univ = a`                    | Laws 2 pre,future     |
| C5     | `emp ≺ a = a`                     | Laws 1,2 pre,future   |
| C6     | `r ≺ univ = univ`                 | Laws 1,2 pre,future   |
| C7     | `(a ⋄ b) ≺ x = a ≺ (b ≺ x)`      | Law 3 pre,future      |
| C8     | `c ≺ (a ⊓ b) = (c ≺ a) ⊓ (c ≺ b)`| Law 3 pre,future      |
| Ccomm  | `(a ⋄ b) ≺ x = (b ⋄ a) ≺ x`      | Law 3 future only     |

**Note on Ccomm**: The informal proof in `Core.hs` implicitly uses Ccomm when
applying C7 to the `future` component of Law 3.  Ccomm does not follow from
C1–C8 alone; it holds for all concrete instances (RE, SL, GuardedRE, WRE)
where post-conditions commute inside residuals.
-/

-- ═══════════════════════════════════════════════════════════════════════════
-- § 1  Composable algebra
-- ═══════════════════════════════════════════════════════════════════════════

/-- Abstract algebra for effect types, matching `class Composable` in Core.hs. -/
class Composable (eff : Type) where
  cat  : eff → eff → eff   -- (·)  sequential composition
  meet : eff → eff → eff   -- (/\) conjunction
  res  : eff → eff → eff   -- res r s = subtraction r s = s \\ r in Haskell
  emp  : eff               -- empty:    unit for cat
  univ : eff               -- universe: unit for meet

infixl:70 " ⋄ " => Composable.cat
infixl:65 " ⊓ " => Composable.meet
infixl:60 " ≺ " => Composable.res   -- r ≺ s  means  s \\ r  in Haskell

-- ═══════════════════════════════════════════════════════════════════════════
-- § 2  Axioms
-- ═══════════════════════════════════════════════════════════════════════════

/-- The ten algebraic laws required for the three monad-law proofs. -/
structure ComposableAxioms (eff : Type) [Composable eff] : Type where
  C1    : ∀ a : eff,         Composable.emp ⋄ a = a
  C2    : ∀ a : eff,         a ⋄ Composable.emp = a
  C3    : ∀ a b c : eff,     (a ⋄ b) ⋄ c = a ⋄ (b ⋄ c)
  C4    : ∀ a : eff,         Composable.univ ⊓ a = a
  C4r   : ∀ a : eff,         a ⊓ Composable.univ = a
  C5    : ∀ a : eff,         Composable.emp ≺ a = a
  C6    : ∀ r : eff,         r ≺ Composable.univ = Composable.univ
  C7    : ∀ a b x : eff,     (a ⋄ b) ≺ x = a ≺ (b ≺ x)
  C8    : ∀ c a b : eff,     c ≺ (a ⊓ b) = (c ≺ a) ⊓ (c ≺ b)
  Ccomm : ∀ a b x : eff,     (a ⋄ b) ≺ x = (b ⋄ a) ≺ x

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
    pre    := p.pre ⊓ (p.post ≺ gp.pre)
    post   := p.post ⋄ gp.post
    future := (gp.post ≺ p.future) ⊓ gp.future }

-- ═══════════════════════════════════════════════════════════════════════════
-- § 5  Monad laws
-- ═══════════════════════════════════════════════════════════════════════════

variable {eff : Type} [Composable eff]
variable {α β γ : Type}

/-!
### Law 1 — Left identity: `pure x >>= g = g x`

Let `g x = (b, P', Q', F')`.

| component | expression       | steps                    |
|-----------|------------------|--------------------------|
| pre    | `univ ⊓ (emp ≺ P')` | `= univ ⊓ P' = P'`   C5, C4 |
| post   | `emp ⋄ Q'`          | `= Q'`               C1     |
| future | `(Q' ≺ univ) ⊓ F'` | `= univ ⊓ F' = F'`   C6, C4 |
-/
theorem pledge_left_id (ax : ComposableAxioms eff) (x : α) (g : α → Pledge eff β) :
    Pledge.bind (Pledge.pure x) g = g x := by
  apply Pledge.ext
  · simp only [Pledge.bind, Pledge.pure]
  · simp only [Pledge.bind, Pledge.pure, ax.C5, ax.C4]
  · simp only [Pledge.bind, Pledge.pure, ax.C1]
  · simp only [Pledge.bind, Pledge.pure, ax.C6, ax.C4]

/-!
### Law 2 — Right identity: `p >>= pure = p`

Let `p = (a, P, Q, F)`.

| component | expression        | steps                     |
|-----------|-------------------|---------------------------|
| pre    | `P ⊓ (Q ≺ univ)`    | `= P ⊓ univ = P`   C6, C4r |
| post   | `Q ⋄ emp`           | `= Q`               C2     |
| future | `(emp ≺ F) ⊓ univ`  | `= F ⊓ univ = F`   C5, C4r |
-/
theorem pledge_right_id (ax : ComposableAxioms eff) (p : Pledge eff α) :
    Pledge.bind p Pledge.pure = p := by
  apply Pledge.ext
  · simp only [Pledge.bind, Pledge.pure]
  · simp only [Pledge.bind, Pledge.pure, ax.C6, ax.C4r]
  · simp only [Pledge.bind, Pledge.pure, ax.C2]
  · simp only [Pledge.bind, Pledge.pure, ax.C5, ax.C4r]

/-!
### Law 3 — Associativity: `(p >>= f) >>= g = p >>= (fun x => f x >>= g)`

Let `p=(a,P,Q,F)`, `f a=(b,P',Q',F')`, `g b=(c,P'',Q'',F'')`.

**LHS** (`(p >>= f) >>= g`):
```
  pre_L    = [P ⊓ (Q ≺ P')] ⊓ [(Q ⋄ Q') ≺ P'']
  post_L   = (Q ⋄ Q') ⋄ Q''
  future_L = (Q'' ≺ [(Q' ≺ F) ⊓ F']) ⊓ F''
```

**RHS** (`p >>= (fun x => f x >>= g)`):
```
  pre_R    = P ⊓ (Q ≺ [P' ⊓ (Q' ≺ P'')])
  post_R   = Q ⋄ (Q' ⋄ Q'')
  future_R = ((Q' ⋄ Q'') ≺ F) ⊓ [(Q'' ≺ F') ⊓ F'']
```

**post**: `post_L = post_R` by C3.

**pre** (rewrite RHS using C8, ←C7, ←meet_assoc):
```
  Q ≺ [P' ⊓ (Q'≺P'')]
    = (Q≺P') ⊓ (Q ≺ (Q'≺P''))   by C8
    = (Q≺P') ⊓ ((Q⋄Q') ≺ P'')   by ←C7
  P ⊓ [(Q≺P') ⊓ ((Q⋄Q')≺P'')]
    = [P⊓(Q≺P')] ⊓ [(Q⋄Q')≺P''] by ←meet_assoc
```

**future** (expand LHS using C8, ←C7, Ccomm, meet_assoc):
```
  Q'' ≺ [(Q'≺F) ⊓ F']
    = (Q''≺(Q'≺F)) ⊓ (Q''≺F')    by C8
    = ((Q''⋄Q')≺F) ⊓ (Q''≺F')   by ←C7  (a=Q'', b=Q', x=F)
    = ((Q'⋄Q'')≺F) ⊓ (Q''≺F')   by Ccomm
  [((Q'⋄Q'')≺F)⊓(Q''≺F')]⊓F''
    = (Q'⋄Q'')≺F ⊓ [(Q''≺F')⊓F''] by meet_assoc
```
-/
theorem pledge_assoc
    (ax : ComposableAxioms eff)
    (meet_assoc : ∀ a b c : eff, (a ⊓ b) ⊓ c = a ⊓ (b ⊓ c))
    (p : Pledge eff α) (f : α → Pledge eff β) (g : β → Pledge eff γ) :
    Pledge.bind (Pledge.bind p f) g =
    Pledge.bind p (fun x => Pledge.bind (f x) g) := by
  apply Pledge.ext
  · -- ret: definitional
    simp only [Pledge.bind]
  · -- pre: [P⊓(Q≺P')]⊓[(Q⋄Q')≺P''] = P⊓(Q≺[P'⊓(Q'≺P'')])
    simp only [Pledge.bind]
    rw [ax.C8, ← ax.C7, ← meet_assoc]
  · -- post: (Q⋄Q')⋄Q'' = Q⋄(Q'⋄Q'')
    simp only [Pledge.bind]
    rw [ax.C3]
  · -- future: (Q''≺[(Q'≺F)⊓F'])⊓F'' = ((Q'⋄Q'')≺F)⊓[(Q''≺F')⊓F'']
    simp only [Pledge.bind]
    rw [ax.C8, ← ax.C7, ax.Ccomm, meet_assoc]

-- ═══════════════════════════════════════════════════════════════════════════
-- § 6  Summary
-- ═══════════════════════════════════════════════════════════════════════════

/-!
## Theorem inventory

| Theorem            | sorry-free? | Axioms consumed           |
|--------------------|-------------|---------------------------|
| `pledge_left_id`   | ✓           | C1, C4, C5, C6            |
| `pledge_right_id`  | ✓           | C2, C4r, C5, C6           |
| `pledge_assoc`     | ✓           | C3, C7, C8, Ccomm, meet_assoc |

All three proofs are `sorry`-free.

## Relationship to `FutureCond.lean`

- `FutureCond.lean` formalises an older `Effectful` design where `pre` uses
  `cat` (`e.pre ⋄ (fe.pre ∖∖ e.post)`).  The current `Pledge/Core.hs` uses
  `meet` (`preA /\\ (preB \\\\ postA)`).  This file formalises the current design.
- `assoc_future` in `FutureCond.lean` carries a `sorry` for the same reason:
  it implicitly needs `Ccomm` which was not listed among C1–C8.
-/
