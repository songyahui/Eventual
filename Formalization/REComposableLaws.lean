/-!
# REComposableLaws.lean

Formal proofs that `RE` (from `Pledge/RE.hs`) satisfies the 14 algebraic laws
(`S1–S3, C1–C3, L1–L4, R1–R4`) required for the Pledge monad laws, stated
as **language containments** (`LContainment`).

`LContainment L1 L2`, written `L1 ⊆ L2`, is the one-way inclusion
`∀ w, L1 w → L2 w`, not bi-implication.  Every law below is read left-to-right
only; where the converse inclusion also happens to hold it is given separately
(`S3'`, `L3'`, `R3'`).

Each law is proved twice: abstractly over languages (§2), and for the `RE`
syntax itself (§6), the latter bundled as `re_satisfies_axioms : REAxioms lq rq`.
The eight quotient laws take the correctness of the quotient operations
(`IsLeftQuotient` / `IsRightQuotient`, the specification of `reLeftQuotient` /
`reRightQuotient`) as an explicit hypothesis; nothing is axiomatised, so
`#print axioms re_satisfies_axioms` reports only `propext` and `Quot.sound`.

## Status

| Law | Lang (§2) | RE (§6) | Notes                                       |
|-----|-----------|---------|---------------------------------------------|
| S1  | `S1`      | `re_S1` | `catL epsL L ⊆ L` (converse also holds)    |
| S2  | `S2`      | `re_S2` | `catL L epsL ⊆ L` (converse also holds)    |
| S3  | `S3`      | `re_S3` | associativity of `catL` (converse: `S3'`)   |
| C1  | `C1`      | `re_C1` | commutativity of `andL`                     |
| C2  | `C2`      | `re_C2` | associativity of `andL`                     |
| C3  | `C3`      | `re_C3` | `andL topL L ⊆ L`                          |
| L1  | `L1`      | `re_L1` | `lqL epsL L ⊆ L`                           |
| L2  | `L2`      | `re_L2` | holds for any divisor, `Bot` included       |
| L3  | `L3`      | `re_L3` | sequential left-quotient (converse: `L3'`)  |
| L4  | `L4`      | `re_L4` | converse fails (§L4)                        |
| R1  | `R1`      | `re_R1` | mirrors L1                                  |
| R2  | `R2`      | `re_R2` | mirrors L2                                  |
| R3  | `R3`      | `re_R3` | mirrors L3 (converse: `R3'`)                |
| R4  | `R4`      | `re_R4` | mirrors L4                                  |
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

/-- **Language containment**: `L1 ⊆ L2` means every word of `L1` is a word of
    `L2`.  Every law below is stated as this one-way inclusion; the converse
    inclusions are not claimed. -/
def LContainment (L1 L2 : Lang α) : Prop := ∀ w, L1 w → L2 w
local infix:50 " ⊆ " => LContainment

def catPow (L : Lang α) : Nat → Lang α
  | 0     => epsL
  | n + 1 => catL L (catPow L n)

def starL (L : Lang α) : Lang α := fun w => ∃ n : Nat, catPow L n w

-- ═══════════════════════════════════════════════════════════════════════════
-- § 2  14 laws at the abstract language level (all sorry-free)
-- ═══════════════════════════════════════════════════════════════════════════

-- ── Sequential algebra (S1–S3) ─────────────────────────────────────────────

theorem S1 (L : Lang α) : catL epsL L ⊆ L := fun w => by
  rintro ⟨u, v, rfl, (rfl : u = []), hv⟩; simpa

theorem S2 (L : Lang α) : catL L epsL ⊆ L := fun w => by
  rintro ⟨u, v, rfl, hu, (rfl : v = [])⟩; simpa

theorem S3 (L1 L2 L3 : Lang α) : catL (catL L1 L2) L3 ⊆ catL L1 (catL L2 L3) := fun w => by
  rintro ⟨_, c, rfl, ⟨a, b, rfl, h1, h2⟩, h3⟩
  exact ⟨a, b ++ c, List.append_assoc a b c, h1, b, c, rfl, h2, h3⟩

/-- The converse inclusion of `S3`, also available since `catL` is genuinely
    associative. -/
theorem S3' (L1 L2 L3 : Lang α) : catL L1 (catL L2 L3) ⊆ catL (catL L1 L2) L3 := fun w => by
  rintro ⟨a, _, rfl, h1, b, c, rfl, h2, h3⟩
  exact ⟨a ++ b, c, (List.append_assoc a b c).symm, ⟨a, b, rfl, h1, h2⟩, h3⟩

-- ── Meet/Conjunction algebra (C1–C3) ──────────────────────────────────────

theorem C1 (L1 L2 : Lang α) : andL L1 L2 ⊆ andL L2 L1 := fun w => by
  rintro ⟨h1, h2⟩; exact ⟨h2, h1⟩

theorem C2 (L1 L2 L3 : Lang α) :
    andL (andL L1 L2) L3 ⊆ andL L1 (andL L2 L3) := fun w => by
  rintro ⟨⟨h1, h2⟩, h3⟩; exact ⟨h1, h2, h3⟩

theorem C3 (L : Lang α) : andL topL L ⊆ L := fun w => by
  rintro ⟨_, h⟩; exact h

-- Right identity `andL L topL ⊆ L` is a corollary of C1 + C3.
theorem C3r (L : Lang α) : andL L topL ⊆ L := fun w => by
  rintro ⟨h, _⟩; exact h

-- ── Left-quotient laws (L1–L4) ─────────────────────────────────────────────

theorem L1 (L : Lang α) : lqL epsL L ⊆ L := fun w => by
  rintro ⟨u, (rfl : u = []), hw⟩; simpa

/-- L2: `Σ* ∖ Ld ⊆ Σ*`, for any divisor `Ld`. -/
theorem L2 (Ld : Lang α) : lqL Ld topL ⊆ topL :=
  fun _ _ => trivial

theorem L3 (La Lb Lx : Lang α) :
    lqL (catL La Lb) Lx ⊆ lqL Lb (lqL La Lx) := fun w => by
  rintro ⟨_, ⟨a, b, rfl, ha, hb⟩, hw⟩
  exact ⟨b, hb, a, ha, List.append_assoc a b w ▸ hw⟩

/-- The converse inclusion of `L3`, also available. -/
theorem L3' (La Lb Lx : Lang α) :
    lqL Lb (lqL La Lx) ⊆ lqL (catL La Lb) Lx := fun w => by
  rintro ⟨b, hb, a, ha, hw⟩
  exact ⟨a ++ b, ⟨a, b, rfl, ha, hb⟩, (List.append_assoc a b w).symm ▸ hw⟩

/-!
### L4 — holds as a containment

Only the left-to-right inclusion is true, which is exactly what the law
asserts.  The converse fails: two witnesses from `Ld` (for `La` and `Lb`)
may differ.

**Counterexample for the converse** (as language sets):
`Ld = {[a],[b]}`, `La = {[a]}`, `Lb = {[b]}`:
- LHS = `∅` (no single `u` satisfies both)
- RHS = `{ε} ∩ {ε} = {ε}`
-/
theorem L4 (Ld La Lb : Lang α) :
    lqL Ld (andL La Lb) ⊆ andL (lqL Ld La) (lqL Ld Lb) := fun w => by
  rintro ⟨u, hu, ha, hb⟩; exact ⟨⟨u, hu, ha⟩, u, hu, hb⟩

-- ── Right-quotient laws (R1–R4) ────────────────────────────────────────────

theorem R1 (L : Lang α) : rqL epsL L ⊆ L := fun w => by
  rintro ⟨u, (rfl : u = []), hw⟩; simpa using hw

/-- R2: mirrors `L2`. -/
theorem R2 (Ld : Lang α) : rqL Ld topL ⊆ topL :=
  fun _ _ => trivial

theorem R3 (La Lb Lx : Lang α) :
    rqL La (rqL Lb Lx) ⊆ rqL (catL La Lb) Lx := fun w => by
  rintro ⟨a, ha, b, hb, hw⟩
  exact ⟨a ++ b, ⟨a, b, rfl, ha, hb⟩, List.append_assoc w a b ▸ hw⟩

/-- The converse inclusion of `R3`, also available. -/
theorem R3' (La Lb Lx : Lang α) :
    rqL (catL La Lb) Lx ⊆ rqL La (rqL Lb Lx) := fun w => by
  rintro ⟨_, ⟨a, b, rfl, ha, hb⟩, hw⟩
  exact ⟨a, ha, b, hb, (List.append_assoc w a b).symm ▸ hw⟩

theorem R4 (Ld La Lb : Lang α) :
    rqL Ld (andL La Lb) ⊆ andL (rqL Ld La) (rqL Ld Lb) := fun w => by
  rintro ⟨u, hu, ha, hb⟩; exact ⟨⟨u, hu, ha⟩, u, hu, hb⟩

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

def RELangContainment (r1 r2 : RE α) : Prop := LContainment (lang r1) (lang r2)

-- ── RE laws S1–S3, C1–C3 (sorry-free) ─────────────────────────────────────

theorem re_S1 (r : RE α) : RELangContainment (RE.Seq .Eps r) r :=
  fun w => by rw [lang_Seq, lang_Eps]; exact S1 (lang r) w

theorem re_S2 (r : RE α) : RELangContainment (RE.Seq r .Eps) r :=
  fun w => by rw [lang_Seq, lang_Eps]; exact S2 (lang r) w

theorem re_S3 (r1 r2 r3 : RE α) :
    RELangContainment (RE.Seq (RE.Seq r1 r2) r3) (RE.Seq r1 (RE.Seq r2 r3)) :=
  fun w => by simp only [lang_Seq]; exact S3 (lang r1) (lang r2) (lang r3) w

theorem re_C1 (r1 r2 : RE α) : RELangContainment (RE.And r1 r2) (RE.And r2 r1) :=
  fun w => by rw [lang_And, lang_And]; exact C1 (lang r1) (lang r2) w

theorem re_C2 (r1 r2 r3 : RE α) :
    RELangContainment (RE.And (RE.And r1 r2) r3) (RE.And r1 (RE.And r2 r3)) :=
  fun w => by simp only [lang_And]; exact C2 (lang r1) (lang r2) (lang r3) w

theorem re_C3 (r : RE α) : RELangContainment (RE.And (.Not .Bot) r) r :=
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
-- § 6  Quotient operations: specification and the 14 RE-level laws
-- ═══════════════════════════════════════════════════════════════════════════

/-!
`reLeftQuotient` / `reRightQuotient` (`Pledge/RE.hs`) are worklist fixpoints
over ACI-equivalence classes of derivative pairs; reproducing that algorithm
and its termination argument is out of scope here.  What the laws actually need
is only the *denotational* specification below, so it is taken as an explicit
hypothesis of every law that mentions a quotient rather than as an axiom —
this file therefore adds nothing to Lean's trusted base.

`derivative_eq_lq_single` discharges the specification on one-letter divisors,
which is exactly the Brzozowski derivative.
-/

/-- `lq` computes the left quotient: `L(lq rd r) = L(rd) ∖ L(r)`. -/
def IsLeftQuotient (lq : RE α → RE α → RE α) : Prop :=
  ∀ rd r w, lang (lq rd r) w ↔ lqL (lang rd) (lang r) w

/-- `rq` computes the right quotient: `L(rq rd r) = L(r) ∕ L(rd)`. -/
def IsRightQuotient (rq : RE α → RE α → RE α) : Prop :=
  ∀ rd r w, lang (rq rd r) w ↔ rqL (lang rd) (lang r) w

theorem IsLeftQuotient.lang_eq {lq : RE α → RE α → RE α}
    (h : IsLeftQuotient lq) (rd r : RE α) :
    lang (lq rd r) = lqL (lang rd) (lang r) :=
  funext fun w => propext (h rd r w)

theorem IsRightQuotient.lang_eq {rq : RE α → RE α → RE α}
    (h : IsRightQuotient rq) (rd r : RE α) :
    lang (rq rd r) = rqL (lang rd) (lang r) :=
  funext fun w => propext (h rd r w)

/-- The specification is inhabited on one-letter divisors: the left quotient by
    `{[a]}` is exactly the Brzozowski derivative `∂_a`. -/
theorem derivative_eq_lq_single (a : α) (r : RE α) :
    lang (derivative a r) = lqL (lang (RE.Single (Event.Atom a))) (lang r) := by
  funext w
  apply propext
  constructor
  · intro h
    exact ⟨[a], ⟨a, rfl, by simp [matchesEvent]⟩, (derivative_correct a r w).mp h⟩
  · rintro ⟨_, ⟨b, rfl, hb⟩, hw⟩
    simp only [matchesEvent, decide_eq_true_eq] at hb
    rw [hb] at hw
    exact (derivative_correct a r w).mpr hw

-- ── The 14 laws for `RE` (S1–S3, C1–C3 unconditional; L*, R* given the spec) ──

variable {lq rq : RE α → RE α → RE α}

theorem re_L1 (hlq : IsLeftQuotient lq) (r : RE α) :
    RELangContainment (lq .Eps r) r := fun w => by
  rw [hlq.lang_eq, lang_Eps]; exact L1 (lang r) w

theorem re_L2 (hlq : IsLeftQuotient lq) (rd : RE α) :
    RELangContainment (lq rd (.Not .Bot)) (.Not .Bot) := fun w => by
  rw [hlq.lang_eq, lang_top]; exact L2 (lang rd) w

theorem re_L3 (hlq : IsLeftQuotient lq) (ra rb rx : RE α) :
    RELangContainment (lq (.Seq ra rb) rx) (lq rb (lq ra rx)) := fun w => by
  simp only [hlq.lang_eq, lang_Seq]
  exact L3 (lang ra) (lang rb) (lang rx) w

theorem re_L4 (hlq : IsLeftQuotient lq) (rd ra rb : RE α) :
    RELangContainment (lq rd (.And ra rb)) (.And (lq rd ra) (lq rd rb)) := fun w => by
  simp only [hlq.lang_eq, lang_And]
  exact L4 (lang rd) (lang ra) (lang rb) w

theorem re_R1 (hrq : IsRightQuotient rq) (r : RE α) :
    RELangContainment (rq .Eps r) r := fun w => by
  rw [hrq.lang_eq, lang_Eps]; exact R1 (lang r) w

theorem re_R2 (hrq : IsRightQuotient rq) (rd : RE α) :
    RELangContainment (rq rd (.Not .Bot)) (.Not .Bot) := fun w => by
  rw [hrq.lang_eq, lang_top]; exact R2 (lang rd) w

theorem re_R3 (hrq : IsRightQuotient rq) (ra rb rx : RE α) :
    RELangContainment (rq ra (rq rb rx)) (rq (.Seq ra rb) rx) := fun w => by
  simp only [hrq.lang_eq, lang_Seq]
  exact R3 (lang ra) (lang rb) (lang rx) w

theorem re_R4 (hrq : IsRightQuotient rq) (rd ra rb : RE α) :
    RELangContainment (rq rd (.And ra rb)) (.And (rq rd ra) (rq rd rb)) := fun w => by
  simp only [hrq.lang_eq, lang_And]
  exact R4 (lang rd) (lang ra) (lang rb) w

-- ── The bundle ─────────────────────────────────────────────────────────────

/-- The 14 laws `S1–S3, C1–C3, L1–L4, R1–R4` of `ComposableAxioms`
    (`PledgeMonadLaws.lean`), instantiated at `RE` and read as containments. -/
structure REAxioms (lq rq : RE α → RE α → RE α) : Prop where
  S1 : ∀ r : RE α, RELangContainment (.Seq .Eps r) r
  S2 : ∀ r : RE α, RELangContainment (.Seq r .Eps) r
  S3 : ∀ r1 r2 r3 : RE α,
        RELangContainment (.Seq (.Seq r1 r2) r3) (.Seq r1 (.Seq r2 r3))
  C1 : ∀ r1 r2 : RE α, RELangContainment (.And r1 r2) (.And r2 r1)
  C2 : ∀ r1 r2 r3 : RE α,
        RELangContainment (.And (.And r1 r2) r3) (.And r1 (.And r2 r3))
  C3 : ∀ r : RE α, RELangContainment (.And (.Not .Bot) r) r
  L1 : ∀ r : RE α, RELangContainment (lq .Eps r) r
  L2 : ∀ rd : RE α, RELangContainment (lq rd (.Not .Bot)) (.Not .Bot)
  L3 : ∀ ra rb rx : RE α, RELangContainment (lq (.Seq ra rb) rx) (lq rb (lq ra rx))
  L4 : ∀ rd ra rb : RE α,
        RELangContainment (lq rd (.And ra rb)) (.And (lq rd ra) (lq rd rb))
  R1 : ∀ r : RE α, RELangContainment (rq .Eps r) r
  R2 : ∀ rd : RE α, RELangContainment (rq rd (.Not .Bot)) (.Not .Bot)
  R3 : ∀ ra rb rx : RE α, RELangContainment (rq ra (rq rb rx)) (rq (.Seq ra rb) rx)
  R4 : ∀ rd ra rb : RE α,
        RELangContainment (rq rd (.And ra rb)) (.And (rq rd ra) (rq rd rb))

/-- **Main result**: any correct pair of quotient operations makes `RE` satisfy
    all 14 laws.  `Pledge/RE.hs` supplies that pair as `reLeftQuotient` /
    `reRightQuotient`. -/
theorem re_satisfies_axioms (hlq : IsLeftQuotient lq) (hrq : IsRightQuotient rq) :
    REAxioms lq rq where
  S1 := re_S1
  S2 := re_S2
  S3 := re_S3
  C1 := re_C1
  C2 := re_C2
  C3 := re_C3
  L1 := re_L1 hlq
  L2 := re_L2 hlq
  L3 := re_L3 hlq
  L4 := re_L4 hlq
  R1 := re_R1 hrq
  R2 := re_R2 hrq
  R3 := re_R3 hrq
  R4 := re_R4 hrq

end REComposableLaws

/-!
## Summary

**Abstract language laws (§2)** — all 14 proved sorry-free.

**RE instance (§3–§6)** — all 14 proved sorry-free and bundled as
`re_satisfies_axioms`:
- `nullable_iff`: sorry-free.
- `derivative_correct`: sorry-free for all cases.
  `Seq` uses `cases u` on the word decomposition; `Star` uses `catPow_cons_decomp`
  (induction on the `catPow` iteration count `n`).
- `derivative_eq_lq_single`: the derivative *is* the left quotient by a
  one-letter divisor, discharging the quotient specification in that case.
- L1–L4 / R1–R4 are conditional on `IsLeftQuotient lq` / `IsRightQuotient rq`,
  the denotational specification of `reLeftQuotient` / `reRightQuotient`.
  That specification is a hypothesis, not an axiom: the worklist fixpoint those
  functions implement is not reproduced here, but assuming it is correct is the
  only thing left unverified, and it is visible in every statement that needs it.

**Reading of `⊆`**: language containment, not equality.  Under this reading all
14 laws hold **unconditionally** — no side conditions on any `RE`.  The
converse inclusions are *not* claimed; for L4 / R4 the converse fails outright
(counterexample in §L4).
-/
