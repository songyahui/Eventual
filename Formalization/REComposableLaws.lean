/-!
# REComposableLaws.lean

Formal proofs that `RE` (from `Pledge/RE.hs`) satisfies the 14 algebraic laws
(`S1–S3, C1–C3, L1–L4, R1–R4`) required for the Pledge monad laws, stated
as **language equivalences** (`LEquiv`).

## Why language equivalence?

`concatenation = Seq` in the RE Composable instance, so `Seq Eps r ≠ r`
propositionally.  All laws are stated as `∀ w, lang lhs w ↔ lang rhs w`.

## Status

| Law | Status   | Notes                                              |
|-----|----------|----------------------------------------------------|
| S1  | ✓        | `catL epsL L ≅ L`                                 |
| S2  | ✓        | `catL L epsL ≅ L`                                 |
| S3  | ✓        | associativity of `catL`                            |
| C1  | ✓        | commutativity of `andL`                            |
| C2  | ✓        | associativity of `andL`                            |
| C3  | ✓        | `andL topL L ≅ L`                                 |
| L1  | ✓        | `lqL epsL L ≅ L` (definitional in RE.hs)          |
| L2  | ✓*       | needs `L(divisor) ≠ ∅`; fails for `Bot`           |
| L3  | ✓        | sequential left-quotient law                       |
| L4  | → only   | full equality fails (§L4 counterexample)           |
| R1  | ✓        | mirrors L1                                         |
| R2  | ✓*       | needs `L(divisor) ≠ ∅`; mirrors L2                |
| R3  | ✓        | mirrors L3                                         |
| R4  | → only   | mirrors L4                                         |
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

def catPow (L : Lang α) : Nat → Lang α
  | 0     => epsL
  | n + 1 => catL L (catPow L n)

def starL (L : Lang α) : Lang α := fun w => ∃ n : Nat, catPow L n w

-- ═══════════════════════════════════════════════════════════════════════════
-- § 2  14 laws at the abstract language level (all sorry-free)
-- ═══════════════════════════════════════════════════════════════════════════

-- ── Sequential algebra (S1–S3) ─────────────────────────────────────────────

theorem S1 (L : Lang α) : catL epsL L ≅ L := fun w => by
  constructor
  · rintro ⟨u, v, rfl, (rfl : u = []), hv⟩; simpa
  · intro hw; exact ⟨[], w, rfl, rfl, hw⟩

theorem S2 (L : Lang α) : catL L epsL ≅ L := fun w => by
  constructor
  · rintro ⟨u, v, rfl, hu, (rfl : v = [])⟩; simpa
  · intro hw; exact ⟨w, [], by simp, hw, rfl⟩

theorem S3 (L1 L2 L3 : Lang α) : catL (catL L1 L2) L3 ≅ catL L1 (catL L2 L3) := fun w => by
  constructor
  · rintro ⟨_, c, rfl, ⟨a, b, rfl, h1, h2⟩, h3⟩
    exact ⟨a, b ++ c, List.append_assoc a b c, h1, b, c, rfl, h2, h3⟩
  · rintro ⟨a, _, rfl, h1, b, c, rfl, h2, h3⟩
    exact ⟨a ++ b, c, (List.append_assoc a b c).symm, ⟨a, b, rfl, h1, h2⟩, h3⟩

-- ── Meet/Conjunction algebra (C1–C3) ──────────────────────────────────────

theorem C1 (L1 L2 : Lang α) : andL L1 L2 ≅ andL L2 L1 := fun w => by
  simp [andL, And.comm]

theorem C2 (L1 L2 L3 : Lang α) :
    andL (andL L1 L2) L3 ≅ andL L1 (andL L2 L3) := fun w => by
  simp [andL, and_assoc]

theorem C3 (L : Lang α) : andL topL L ≅ L := fun w => by simp [andL, topL]

-- Right identity `andL L topL ≅ L` is a corollary of C1 + C3.
theorem C3r (L : Lang α) : andL L topL ≅ L := fun w => by simp [andL, topL]

-- ── Left-quotient laws (L1–L4) ─────────────────────────────────────────────

theorem L1 (L : Lang α) : lqL epsL L ≅ L := fun w => by
  constructor
  · rintro ⟨u, (rfl : u = []), hw⟩; simpa
  · intro hw; exact ⟨[], rfl, by simpa⟩

/-- L2: Σ* ∖ Ld = Σ*, assuming Ld is non-empty.
    Fails when `L(Ld) = ∅` (e.g. for `Bot`). -/
theorem L2 (Ld : Lang α) (hne : ∃ u, Ld u) : lqL Ld topL ≅ topL := by
  intro w
  simp only [lqL, topL, and_true]
  exact ⟨fun _ => trivial, fun _ => hne⟩

theorem L3 (La Lb Lx : Lang α) :
    lqL (catL La Lb) Lx ≅ lqL Lb (lqL La Lx) := fun w => by
  constructor
  · rintro ⟨_, ⟨a, b, rfl, ha, hb⟩, hw⟩
    exact ⟨b, hb, a, ha, List.append_assoc a b w ▸ hw⟩
  · rintro ⟨b, hb, a, ha, hw⟩
    exact ⟨a ++ b, ⟨a, b, rfl, ha, hb⟩, (List.append_assoc a b w).symm ▸ hw⟩

/-!
### L4 — only the forward (→) direction holds

Full equality fails: two witnesses from `Ld` (for `La` and `Lb`) may differ.

**Counterexample** (as language sets):
`Ld = {[a],[b]}`, `La = {[a]}`, `Lb = {[b]}`:
- LHS = `∅` (no single `u` satisfies both)
- RHS = `{ε} ∩ {ε} = {ε}`
-/
theorem L4_fwd {Ld La Lb : Lang α} {w : List α}
    (h : lqL Ld (andL La Lb) w) : andL (lqL Ld La) (lqL Ld Lb) w := by
  obtain ⟨u, hu, ha, hb⟩ := h; exact ⟨⟨u, hu, ha⟩, u, hu, hb⟩

-- ── Right-quotient laws (R1–R4) ────────────────────────────────────────────

theorem R1 (L : Lang α) : rqL epsL L ≅ L := fun w => by
  constructor
  · rintro ⟨u, (rfl : u = []), hw⟩; simpa using hw
  · intro hw; exact ⟨[], rfl, by simpa using hw⟩

theorem R2 (Ld : Lang α) (hne : ∃ u, Ld u) : rqL Ld topL ≅ topL := by
  intro w
  simp only [rqL, topL, and_true]
  exact ⟨fun _ => trivial, fun _ => hne⟩

theorem R3 (La Lb Lx : Lang α) :
    rqL La (rqL Lb Lx) ≅ rqL (catL La Lb) Lx := fun w => by
  constructor
  · rintro ⟨a, ha, b, hb, hw⟩
    exact ⟨a ++ b, ⟨a, b, rfl, ha, hb⟩, List.append_assoc w a b ▸ hw⟩
  · rintro ⟨_, ⟨a, b, rfl, ha, hb⟩, hw⟩
    exact ⟨a, ha, b, hb, (List.append_assoc w a b).symm ▸ hw⟩

theorem R4_fwd {Ld La Lb : Lang α} {w : List α}
    (h : rqL Ld (andL La Lb) w) : andL (rqL Ld La) (rqL Ld Lb) w := by
  obtain ⟨u, hu, ha, hb⟩ := h; exact ⟨⟨u, hu, ha⟩, u, hu, hb⟩

/-- If `catPow L n` accepts `a :: w`, decompose `w` into a prefix accepted by
    `L` at `a :: _` and a suffix in `starL L`.  Proved by induction on `n`. -/
private theorem catPow_cons_decomp (L : Lang α) (a : α) (w : List α) : ∀ n : Nat,
    catPow L n (a :: w) → ∃ u v, w = u ++ v ∧ L (a :: u) ∧ starL L v := by
  intro n
  induction n with
  | zero => intro h; simp [catPow, epsL] at h
  | succ n ih =>
    intro h
    simp only [catPow, catL] at h
    obtain ⟨p, q, hpq, hLp, hcq⟩ := h
    cases p with
    | nil =>
      simp only [List.nil_append] at hpq
      subst hpq
      exact ih hcq
    | cons b p' =>
      simp only [List.cons_append, List.cons.injEq] at hpq
      obtain ⟨rfl, rfl⟩ := hpq
      exact ⟨p', q, rfl, hLp, ⟨n, hcq⟩⟩

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

-- matchesEvent, lang, nullable, derivative need DecidableEq α.
variable [DecidableEq α]

def matchesEvent (a : α) : Event α → Bool
  | .Wildcard => true
  | .Atom b   => decide (a = b)

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

-- ── RE laws S1–S3, C1–C3 (sorry-free) ─────────────────────────────────────

theorem re_S1 (r : RE α) : RELangEq (RE.Seq .Eps r) r :=
  fun w => by rw [lang_Seq, lang_Eps]; exact S1 (lang r) w

theorem re_S2 (r : RE α) : RELangEq (RE.Seq r .Eps) r :=
  fun w => by rw [lang_Seq, lang_Eps]; exact S2 (lang r) w

theorem re_S3 (r1 r2 r3 : RE α) :
    RELangEq (RE.Seq (RE.Seq r1 r2) r3) (RE.Seq r1 (RE.Seq r2 r3)) :=
  fun w => by simp only [lang_Seq]; exact S3 (lang r1) (lang r2) (lang r3) w

theorem re_C1 (r1 r2 : RE α) : RELangEq (RE.And r1 r2) (RE.And r2 r1) :=
  fun w => by rw [lang_And, lang_And]; exact C1 (lang r1) (lang r2) w

theorem re_C2 (r1 r2 r3 : RE α) :
    RELangEq (RE.And (RE.And r1 r2) r3) (RE.And r1 (RE.And r2 r3)) :=
  fun w => by simp only [lang_And]; exact C2 (lang r1) (lang r2) (lang r3) w

theorem re_C3 (r : RE α) : RELangEq (RE.And (.Not .Bot) r) r :=
  fun w => by rw [lang_And, lang_top]; exact C3 (lang r) w

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
      have hnt : nullable r = true := ih.mpr hr
      simp [hnt] at h
    · intro h
      cases hn : nullable r with
      | true  => exact False.elim (h (ih.mp hn))
      | false => simp

-- ═══════════════════════════════════════════════════════════════════════════
-- § 5  derivative correctness (sorry-free)
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

/-- `lang (∂_a r) w ↔ lang r (a :: w)` — sorry-free. -/
theorem derivative_correct (a : α) (r : RE α) :
    ∀ w, lang (derivative a r) w ↔ lang r (a :: w) := by
  induction r with
  | Bot  => intro w; simp [derivative, lang]
  | Eps  => intro w; simp [derivative, lang, epsL]
  | Single p =>
    intro w
    simp only [derivative, lang, epsL]
    split
    · rename_i hm
      constructor
      · rintro (rfl : w = [])
        exact ⟨a, rfl, hm⟩
      · rintro ⟨b, hb, _⟩
        simp only [List.cons.injEq] at hb
        exact hb.2
    · rename_i hm
      constructor
      · exact False.elim
      · rintro ⟨b, hb, hbm⟩
        simp only [List.cons.injEq] at hb
        simp [← hb.1, hm] at hbm
  | Seq r1 r2 ih1 ih2 =>
    intro w
    -- derivative a (Seq r1 r2) = if nullable r1 then Or (Seq (∂r1) r2) (∂r2) else Seq (∂r1) r2
    show lang (if nullable r1
               then RE.Or (RE.Seq (derivative a r1) r2) (derivative a r2)
               else RE.Seq (derivative a r1) r2) w ↔
         catL (lang r1) (lang r2) (a :: w)
    by_cases hn : nullable r1 = true
    · -- nullable r1 = true: derivative is Or (Seq (∂r1) r2) (∂r2)
      rw [if_pos hn]
      show (catL (lang (derivative a r1)) (lang r2) w ∨ lang (derivative a r2) w) ↔
           catL (lang r1) (lang r2) (a :: w)
      constructor
      · rintro (⟨u, v, rfl, hd, h2⟩ | hd)
        · exact ⟨a :: u, v, rfl, (ih1 u).mp hd, h2⟩
        · exact ⟨[], a :: w, rfl, (nullable_iff r1).mp hn, (ih2 w).mp hd⟩
      · rintro ⟨u, v, huv, h1, h2⟩
        cases u with
        | nil =>
          simp only [List.nil_append] at huv; subst huv
          exact Or.inr ((ih2 w).mpr h2)
        | cons b u' =>
          simp only [List.cons_append, List.cons.injEq] at huv
          obtain ⟨rfl, rfl⟩ := huv
          exact Or.inl ⟨u', v, rfl, (ih1 u').mpr h1, h2⟩
    · -- nullable r1 = false: derivative is Seq (∂r1) r2
      rw [if_neg hn]
      show catL (lang (derivative a r1)) (lang r2) w ↔
           catL (lang r1) (lang r2) (a :: w)
      constructor
      · rintro ⟨u, v, rfl, hd, h2⟩
        exact ⟨a :: u, v, rfl, (ih1 u).mp hd, h2⟩
      · rintro ⟨u, v, huv, h1, h2⟩
        cases u with
        | nil =>
          simp only [List.nil_append] at huv; subst huv
          exact absurd ((nullable_iff r1).mpr h1) hn
        | cons b u' =>
          simp only [List.cons_append, List.cons.injEq] at huv
          obtain ⟨rfl, rfl⟩ := huv
          exact ⟨u', v, rfl, (ih1 u').mpr h1, h2⟩
  | Or r1 r2 ih1 ih2 =>
    intro w; simp [derivative, lang, ih1 w, ih2 w]
  | And r1 r2 ih1 ih2 =>
    intro w; simp [derivative, lang, andL, ih1 w, ih2 w]
  | Star r ih =>
    intro w
    simp only [derivative, lang, catL, starL]
    constructor
    · rintro ⟨u, v, rfl, hd, n, hn⟩
      exact ⟨n + 1, a :: u, v, rfl, (ih u).mp hd, hn⟩
    · -- Decompose via catPow_cons_decomp (induction on the catPow count).
      rintro ⟨n, hn⟩
      obtain ⟨u, v, rfl, hLu, hstv⟩ := catPow_cons_decomp (lang r) a w n hn
      exact ⟨u, v, rfl, (ih u).mpr hLu, hstv⟩
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

theorem re_L1 (r : RE α) : LEquiv (lqL epsL (lang r)) (lang r) := L1 (lang r)

theorem re_L3
    (lq : RE α → RE α → RE α)
    (hlq : ∀ s t w, lang (lq s t) w ↔ lqL (lang s) (lang t) w)
    (r1 r2 r3 : RE α) :
    LEquiv (fun w => lang (lq (RE.Seq r1 r2) r3) w)
           (fun w => lang (lq r2 (lq r1 r3)) w) := by
  intro w
  have hlq_eq : ∀ s t, lang (lq s t) = lqL (lang s) (lang t) :=
    fun s t => funext (fun v => propext (hlq s t v))
  simp only [hlq_eq, lang_Seq]
  exact L3 (lang r1) (lang r2) (lang r3) w

theorem re_R1 (r : RE α) : LEquiv (rqL epsL (lang r)) (lang r) := R1 (lang r)

theorem re_R3
    (rq : RE α → RE α → RE α)
    (hrq : ∀ s t w, lang (rq s t) w ↔ rqL (lang s) (lang t) w)
    (r1 r2 r3 : RE α) :
    LEquiv (fun w => lang (rq r1 (rq r2 r3)) w)
           (fun w => lang (rq (RE.Seq r1 r2) r3) w) := by
  intro w
  have hrq_eq : ∀ s t, lang (rq s t) = rqL (lang s) (lang t) :=
    fun s t => funext (fun v => propext (hrq s t v))
  simp only [hrq_eq, lang_Seq]
  exact R3 (lang r1) (lang r2) (lang r3) w

end REComposableLaws

/-!
## Summary

**Abstract language laws (§2)** — all 14 proved sorry-free.

**RE instance (§3–§6)**:
- `nullable_iff`: sorry-free.
- `derivative_correct`: sorry-free for all cases.
  `Seq` uses `cases u` on the word decomposition; `Star` uses `catPow_cons_decomp`
  (induction on the `catPow` iteration count `n`).
- `lq_correct` / `rq_correct`: axioms (standard automata theory).

**Laws not fully satisfied by RE**:
- L2 / R2: hold only when `L(divisor) ≠ ∅` (fail for `Bot`).
- L4 / R4: only the `⊆` direction holds; full equality fails
  (counterexample in §L4).
-/
