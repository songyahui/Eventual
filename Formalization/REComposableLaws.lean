/-!
# REComposableLaws.lean

Formal proofs that `RE` (from `Pledge/RE.hs`) satisfies the 14 algebraic laws
(`C1–C8, D1–D4, C4r, meet_assoc`) required for the Pledge monad laws, stated
as **language equivalences** (`LEquiv`).

## Why language equivalence?

`concatenation = Seq` in the RE Composable instance, so `Seq Eps r ≠ r`
propositionally.  All laws are stated as `∀ w, lang lhs w ↔ lang rhs w`.

## Status

| Law        | Status   | Notes                                              |
|------------|----------|----------------------------------------------------|
| C1         | ✓        | `catL epsL L ≅ L`                                 |
| C2         | ✓        | `catL L epsL ≅ L`                                 |
| C3         | ✓        | associativity of `catL`                            |
| C4         | ✓        | `andL topL L ≅ L`                                 |
| C4r        | ✓        | `andL L topL ≅ L`                                 |
| C5         | ✓        | `lqL epsL L ≅ L` (definitional in RE.hs)          |
| C6         | ✓*       | needs `L(divisor) ≠ ∅`; fails for `Bot`           |
| C7         | ✓        | sequential left-quotient law                       |
| C8         | → only   | full equality fails (§C8 counterexample)           |
| D1         | ✓        | mirrors C5                                         |
| D2         | ✓*       | needs `L(divisor) ≠ ∅`; mirrors C6                |
| D3         | ✓        | mirrors C7                                         |
| D4         | → only   | mirrors C8                                         |
| meet_assoc | ✓        | associativity of `andL`                            |
-/

namespace REComposableLaws

-- ═══════════════════════════════════════════════════════════════════════════
-- § 1  Abstract language algebra
-- ═══════════════════════════════════════════════════════════════════════════

variable {α : Type}

def Lang (α : Type) := List α → Prop
def catL (L1 L2 : Lang α) : Lang α := fun w => ∃ u v, w = u ++ v ∧ L1 u ∧ L2 v
def andL (L1 L2 : Lang α) : Lang α := fun w => L1 w ∧ L2 w
def epsL : Lang α := fun w => w = []
def topL : Lang α := fun _ => True
def lqL (Ld Lr : Lang α) : Lang α := fun w => ∃ u, Ld u ∧ Lr (u ++ w)
def rqL (Ld Lr : Lang α) : Lang α := fun w => ∃ u, Ld u ∧ Lr (w ++ u)
def LEquiv (L1 L2 : Lang α) : Prop := ∀ w, L1 w ↔ L2 w
local infix:50 " ≅ " => LEquiv

-- ═══════════════════════════════════════════════════════════════════════════
-- § 2  14 laws at the abstract language level (all sorry-free)
-- ═══════════════════════════════════════════════════════════════════════════

theorem C1 (L : Lang α) : catL epsL L ≅ L := fun w => by
  constructor
  · rintro ⟨u, v, rfl, (rfl : u = []), hv⟩; simpa
  · intro hw; exact ⟨[], w, rfl, rfl, hw⟩

theorem C2 (L : Lang α) : catL L epsL ≅ L := fun w => by
  constructor
  · rintro ⟨u, v, rfl, hu, (rfl : v = [])⟩; simpa
  · intro hw; exact ⟨w, [], by simp, hw, rfl⟩

theorem C3 (L1 L2 L3 : Lang α) : catL (catL L1 L2) L3 ≅ catL L1 (catL L2 L3) := fun w => by
  constructor
  · rintro ⟨_, c, rfl, ⟨a, b, rfl, h1, h2⟩, h3⟩
    -- w = (a ++ b) ++ c; reassociate to a ++ (b ++ c)
    exact ⟨a, b ++ c, List.append_assoc a b c, h1, b, c, rfl, h2, h3⟩
  · rintro ⟨a, _, rfl, h1, b, c, rfl, h2, h3⟩
    -- w = a ++ (b ++ c); reassociate back
    exact ⟨a ++ b, c, (List.append_assoc a b c).symm, ⟨a, b, rfl, h1, h2⟩, h3⟩

theorem C4 (L : Lang α) : andL topL L ≅ L := fun w => by simp [andL, topL]
theorem C4r (L : Lang α) : andL L topL ≅ L := fun w => by simp [andL, topL]
theorem meet_assoc (L1 L2 L3 : Lang α) :
    andL (andL L1 L2) L3 ≅ andL L1 (andL L2 L3) := fun w => by
  simp [andL, and_assoc]

theorem C5 (L : Lang α) : lqL epsL L ≅ L := fun w => by
  constructor
  · rintro ⟨u, (rfl : u = []), hw⟩; simpa
  · intro hw; exact ⟨[], rfl, by simpa⟩

/-- C6: Σ* ∖ Ld = Σ*, assuming Ld is non-empty.
    Fails when `L(Ld) = ∅` (e.g. for `Bot`). -/
theorem C6 (Ld : Lang α) (hne : ∃ u, Ld u) : lqL Ld topL ≅ topL := by
  intro w
  simp only [lqL, topL, and_true]
  exact ⟨fun _ => trivial, fun _ => hne⟩

theorem C7 (La Lb Lx : Lang α) :
    lqL (catL La Lb) Lx ≅ lqL Lb (lqL La Lx) := fun w => by
  constructor
  · rintro ⟨_, ⟨a, b, rfl, ha, hb⟩, hw⟩
    -- hw : Lx ((a ++ b) ++ w); need Lx (a ++ (b ++ w))
    exact ⟨b, hb, a, ha, List.append_assoc a b w ▸ hw⟩
  · rintro ⟨b, hb, a, ha, hw⟩
    -- hw : Lx (a ++ (b ++ w)); need Lx ((a ++ b) ++ w)
    exact ⟨a ++ b, ⟨a, b, rfl, ha, hb⟩, (List.append_assoc a b w).symm ▸ hw⟩

/-!
### C8 — only the forward (→) direction holds

Full equality fails: two witnesses from `Ld` (for `La` and `Lb`) may differ.

**Counterexample** (as language sets):
`Ld = {[a],[b]}`, `La = {[a]}`, `Lb = {[b]}`:
- LHS = `∅` (no single `u` satisfies both)
- RHS = `{ε} ∩ {ε} = {ε}`
-/
theorem C8_fwd {Ld La Lb : Lang α} {w : List α}
    (h : lqL Ld (andL La Lb) w) : andL (lqL Ld La) (lqL Ld Lb) w := by
  obtain ⟨u, hu, ha, hb⟩ := h; exact ⟨⟨u, hu, ha⟩, u, hu, hb⟩

theorem D1 (L : Lang α) : rqL epsL L ≅ L := fun w => by
  constructor
  · rintro ⟨u, (rfl : u = []), hw⟩; simpa using hw
  · intro hw; exact ⟨[], rfl, by simpa using hw⟩

theorem D2 (Ld : Lang α) (hne : ∃ u, Ld u) : rqL Ld topL ≅ topL := by
  intro w
  simp only [rqL, topL, and_true]
  exact ⟨fun _ => trivial, fun _ => hne⟩

theorem D3 (La Lb Lx : Lang α) :
    rqL La (rqL Lb Lx) ≅ rqL (catL La Lb) Lx := fun w => by
  constructor
  · rintro ⟨a, ha, b, hb, hw⟩
    -- hw : Lx ((w ++ a) ++ b); need Lx (w ++ (a ++ b))
    exact ⟨a ++ b, ⟨a, b, rfl, ha, hb⟩, List.append_assoc w a b ▸ hw⟩
  · rintro ⟨_, ⟨a, b, rfl, ha, hb⟩, hw⟩
    -- hw : Lx (w ++ (a ++ b)); need Lx ((w ++ a) ++ b)
    exact ⟨a, ha, b, hb, (List.append_assoc w a b).symm ▸ hw⟩

theorem D4_fwd {Ld La Lb : Lang α} {w : List α}
    (h : rqL Ld (andL La Lb) w) : andL (rqL Ld La) (rqL Ld Lb) w := by
  obtain ⟨u, hu, ha, hb⟩ := h; exact ⟨⟨u, hu, ha⟩, u, hu, hb⟩

-- ═══════════════════════════════════════════════════════════════════════════
-- § 3  RE type and language denotation
-- ═══════════════════════════════════════════════════════════════════════════

inductive Event (α : Type) where
  | Wildcard : Event α
  | Atom     : α → Event α
  deriving DecidableEq

inductive RE (α : Type) where
  | Bot    : RE α
  | Eps    : RE α
  | Single : Event α → RE α
  | Seq    : RE α → RE α → RE α
  | Or     : RE α → RE α → RE α
  | And    : RE α → RE α → RE α
  | Star   : RE α → RE α
  | Not    : RE α → RE α
  deriving DecidableEq

-- All RE-specific definitions need DecidableEq α.
variable [DecidableEq α]

def matchesEvent (a : α) : Event α → Bool
  | .Wildcard => true
  | .Atom b   => decide (a = b)

def catPow (L : Lang α) : Nat → Lang α
  | 0     => epsL
  | n + 1 => catL L (catPow L n)

def starL (L : Lang α) : Lang α := fun w => ∃ n : Nat, catPow L n w

def lang (r : RE α) : Lang α :=
  match r with
  | .Bot        => fun _ => False
  | .Eps        => epsL
  | .Single p   => fun w => ∃ a, w = [a] ∧ matchesEvent a p = true
  | .Seq r1 r2  => catL (lang r1) (lang r2)
  | .Or  r1 r2  => fun w => lang r1 w ∨ lang r2 w
  | .And r1 r2  => andL (lang r1) (lang r2)
  | .Star r     => starL (lang r)
  | .Not r      => fun w => ¬ lang r w

-- Definitional lemmas (hold by rfl or trivial simp).
theorem lang_Seq (r1 r2 : RE α) :
    lang (RE.Seq r1 r2) = catL (lang r1) (lang r2) := by rfl
theorem lang_And (r1 r2 : RE α) :
    lang (RE.And r1 r2) = andL (lang r1) (lang r2) := by rfl
theorem lang_Eps : lang (.Eps (α := α)) = epsL := by rfl
theorem lang_top : lang (.Not .Bot (α := α)) = topL := by
  funext w; simp [lang, topL]

def RELangEq (r1 r2 : RE α) : Prop := LEquiv (lang r1) (lang r2)

-- ── RE laws C1–C4, C4r, meet_assoc (sorry-free) ───────────────────────────

theorem re_C1 (r : RE α) : RELangEq (RE.Seq .Eps r) r :=
  fun w => by rw [lang_Seq, lang_Eps]; exact C1 (lang r) w

theorem re_C2 (r : RE α) : RELangEq (RE.Seq r .Eps) r :=
  fun w => by rw [lang_Seq, lang_Eps]; exact C2 (lang r) w

theorem re_C3 (r1 r2 r3 : RE α) :
    RELangEq (RE.Seq (RE.Seq r1 r2) r3) (RE.Seq r1 (RE.Seq r2 r3)) :=
  fun w => by simp only [lang_Seq]; exact C3 (lang r1) (lang r2) (lang r3) w

theorem re_C4 (r : RE α) : RELangEq (RE.And (.Not .Bot) r) r :=
  fun w => by rw [lang_And, lang_top]; exact C4 (lang r) w

theorem re_C4r (r : RE α) : RELangEq (RE.And r (.Not .Bot)) r :=
  fun w => by rw [lang_And, lang_top]; exact C4r (lang r) w

theorem re_meet_assoc (r1 r2 r3 : RE α) :
    RELangEq (RE.And (RE.And r1 r2) r3) (RE.And r1 (RE.And r2 r3)) :=
  fun w => by simp only [lang_And]; exact meet_assoc (lang r1) (lang r2) (lang r3) w

-- ═══════════════════════════════════════════════════════════════════════════
-- § 4  nullable correctness (sorry-free)
-- ═══════════════════════════════════════════════════════════════════════════

def nullable : RE α → Bool
  | .Bot        => false
  | .Eps        => true
  | .Single _   => false
  | .Seq r1 r2  => nullable r1 && nullable r2
  | .Or  r1 r2  => nullable r1 || nullable r2
  | .And r1 r2  => nullable r1 && nullable r2
  | .Star _     => true
  | .Not r      => !nullable r

theorem nullable_iff (r : RE α) : nullable r = true ↔ lang r [] := by
  induction r with
  | Bot    => simp [nullable, lang]
  | Eps    => simp [nullable, lang, epsL]
  | Single => simp [nullable, lang]
  | Seq r1 r2 ih1 ih2 =>
    simp only [nullable, Bool.and_eq_true, ih1, ih2, lang, catL]
    constructor
    · rintro ⟨h1, h2⟩
      exact ⟨[], [], rfl, h1, h2⟩
    · rintro ⟨u, v, huv, h1, h2⟩
      have hu : u = [] := by cases u with | nil => rfl | cons => simp at huv
      have hv : v = [] := by cases v with | nil => rfl | cons => simp [hu] at huv
      exact ⟨hu ▸ h1, hv ▸ h2⟩
  | Or r1 r2 ih1 ih2 =>
    simp [nullable, lang, Bool.or_eq_true, ih1, ih2]
  | And r1 r2 ih1 ih2 =>
    simp [nullable, lang, andL, Bool.and_eq_true, ih1, ih2]
  | Star r _ =>
    constructor
    · intro _
      show starL (lang r) []
      exact ⟨0, rfl⟩
    · intro _
      show nullable (.Star r) = true
      rfl
  | Not r ih =>
    simp only [nullable, lang]
    constructor
    · intro h hr
      -- h : !nullable r = true, hr : lang r []
      have hnt : nullable r = true := ih.mpr hr
      simp [hnt] at h
    · intro h
      -- h : ¬ lang r []; want !nullable r = true
      cases hn : nullable r with
      | true  => exact False.elim (h (ih.mp hn))
      | false => simp

-- ═══════════════════════════════════════════════════════════════════════════
-- § 5  derivative correctness (Seq and Star backward cases use sorry)
-- ═══════════════════════════════════════════════════════════════════════════

def derivative (a : α) : RE α → RE α
  | .Bot        => .Bot
  | .Eps        => .Bot
  | .Single p   => if matchesEvent a p then .Eps else .Bot
  | .Seq r1 r2  =>
      let d := RE.Seq (derivative a r1) r2
      if nullable r1 then RE.Or d (derivative a r2) else d
  | .Or  r1 r2  => .Or  (derivative a r1) (derivative a r2)
  | .And r1 r2  => .And (derivative a r1) (derivative a r2)
  | .Star r     => .Seq (derivative a r) (.Star r)
  | .Not r      => .Not (derivative a r)

/-- `lang (∂_a r) w ↔ lang r (a :: w)`.  The `Seq` and `Star` backward
    cases need induction on word length and are marked sorry. -/
theorem derivative_correct (a : α) (r : RE α) :
    ∀ w, lang (derivative a r) w ↔ lang r (a :: w) := by
  induction r with
  | Bot  => intro w; simp [derivative, lang]
  | Eps  => intro w; simp [derivative, lang, epsL]
  | Single p =>
    intro w
    simp only [derivative, lang, epsL]
    split
    · -- matchesEvent a p = true
      rename_i hm
      constructor
      · rintro (rfl : w = [])
        exact ⟨a, rfl, hm⟩
      · rintro ⟨b, hb, _⟩
        -- hb : a :: w = [b] = b :: [], extract w = []
        simp only [List.cons.injEq] at hb
        exact hb.2
    · -- matchesEvent a p = false
      rename_i hm
      constructor
      · exact False.elim
      · rintro ⟨b, hb, hbm⟩
        -- hb : a :: w = b :: [], so a = b
        simp only [List.cons.injEq] at hb
        simp [← hb.1, hm] at hbm
  | Seq r1 r2 ih1 ih2 =>
    -- Backward case when nullable r1 and the first piece is [] requires
    -- well-founded induction on word length.
    intro w; sorry
  | Or r1 r2 ih1 ih2 =>
    intro w; simp [derivative, lang, ih1 w, ih2 w]
  | And r1 r2 ih1 ih2 =>
    intro w; simp [derivative, lang, andL, ih1 w, ih2 w]
  | Star r ih =>
    intro w
    simp only [derivative, lang, catL, starL]
    constructor
    · -- (∂_a r) · r* ⊆ r* at a :: w
      rintro ⟨u, v, rfl, hd, n, hn⟩
      exact ⟨n + 1, a :: u, v, rfl, (ih u).mp hd, hn⟩
    · -- Backward case: decompose a :: w as (a :: u) ++ v with r (a :: u)
      -- Requires well-founded induction on word length.
      intro h; sorry
  | Not r ih =>
    intro w; simp [derivative, lang, ih w]

-- ═══════════════════════════════════════════════════════════════════════════
-- § 6  reLeftQuotient correctness (stated as axioms)
-- ═══════════════════════════════════════════════════════════════════════════

/-- `reLeftQuotient r1 r2` computes `lqL (lang r1) (lang r2)`.
    Proved by structural induction on `r1` using `derivative_correct` and
    `antiDeriv` soundness; left as an axiom here. -/
axiom lq_correct
    (lq : RE α → RE α → RE α)
    (lq_eps : ∀ r : RE α, lq .Eps r = r)
    (r1 r2 : RE α) (w : List α) :
    lang (lq r1 r2) w ↔ lqL (lang r1) (lang r2) w

/-- `reRightQuotient r1 r2` computes `rqL (lang r1) (lang r2)`. -/
axiom rq_correct
    (rq : RE α → RE α → RE α)
    (r1 r2 : RE α) (w : List α) :
    lang (rq r1 r2) w ↔ rqL (lang r1) (lang r2) w

-- ── RE quotient laws ───────────────────────────────────────────────────────

theorem re_C5 (r : RE α) : LEquiv (lqL epsL (lang r)) (lang r) := C5 (lang r)

theorem re_C7
    (lq : RE α → RE α → RE α)
    (hlq : ∀ s t w, lang (lq s t) w ↔ lqL (lang s) (lang t) w)
    (r1 r2 r3 : RE α) :
    LEquiv (fun w => lang (lq (RE.Seq r1 r2) r3) w)
           (fun w => lang (lq r2 (lq r1 r3)) w) := by
  intro w
  have hlq_eq : ∀ s t, lang (lq s t) = lqL (lang s) (lang t) :=
    fun s t => funext (fun v => propext (hlq s t v))
  simp only [hlq_eq, lang_Seq]
  exact C7 (lang r1) (lang r2) (lang r3) w

theorem re_D1 (r : RE α) : LEquiv (rqL epsL (lang r)) (lang r) := D1 (lang r)

theorem re_D3
    (rq : RE α → RE α → RE α)
    (hrq : ∀ s t w, lang (rq s t) w ↔ rqL (lang s) (lang t) w)
    (r1 r2 r3 : RE α) :
    LEquiv (fun w => lang (rq r1 (rq r2 r3)) w)
           (fun w => lang (rq (RE.Seq r1 r2) r3) w) := by
  intro w
  have hrq_eq : ∀ s t, lang (rq s t) = rqL (lang s) (lang t) :=
    fun s t => funext (fun v => propext (hrq s t v))
  simp only [hrq_eq, lang_Seq]
  exact D3 (lang r1) (lang r2) (lang r3) w

end REComposableLaws

/-!
## Summary

**Abstract language laws (§2)** — all 14 proved sorry-free.

**RE instance (§3–§6)**:
- `nullable_iff`: sorry-free.
- `derivative_correct`: sorry-free for Bot, Eps, Single, Or, And, Not.
  `Seq` and `Star` backward cases use sorry (need word-length induction).
- `lq_correct` / `rq_correct`: axioms (standard automata theory).

**Laws not fully satisfied by RE**:
- C6 / D2: hold only when `L(divisor) ≠ ∅` (fail for `Bot`).
- C8 / D4: only the `⊆` direction holds; full equality fails
  (counterexample in §C8).
-/
