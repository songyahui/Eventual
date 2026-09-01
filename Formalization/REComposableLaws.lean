/-!
# REComposableLaws.lean

Companion to `PledgeMonadLaws.lean`: that file proves the Pledge monad laws
from 14 *abstract* `ComposableAxioms` over an opaque `eff`; this file discharges
those 14 for the concrete `instance Composable (RE t)` of `Pledge/RE.hs`
(`concatenation = Seq`, `conjunction = And`, `empty = Epsilon`,
`universe = top`, `leftQuotient = reLeftQuotient`,
`rightQuotient = reRightQuotient`).

Two readings of each law are given:

* **§2, §6 — containment** (`LContainment`, written `L1 ⊆ L2`, the one-way
  `∀ w, L1 w → L2 w`).  Under this reading all 14 hold *unconditionally*,
  bundled as `re_satisfies_axioms : REAxioms lq rq`.  The quotient laws take
  the spec of `reLeftQuotient` / `reRightQuotient` (`IsLeftQuotient` /
  `IsRightQuotient`) as an explicit hypothesis — nothing is axiomatised.

* **§7 — equality** (`RELEq`, language equality, `∀ w, L1 w ↔ L2 w`).  This is
  the reading `ComposableAxioms` actually demands (`PledgeMonadLaws` `rw`s with
  each law).  Bundled as `Eq7.re_equality_laws : Eq7.REEqualityLaws lq rq`.
  **Here the RE implementation does *not* give every law for free**:
  `L2 R2` fail unless the divisor is satisfiable, `L4 R4` fail unless the
  divisor denotes a single word — each shipped with an explicit
  `Eq7.re_*_not_equality` counterexample and an `iff` / side-condition
  characterisation.  Only `L1` (and `R1`, modulo `revRE` involution) is
  syntactic; `S3 C1 C2` need the language reading; `L3 R3` additionally need
  the quotient spec.

`#print axioms` reports only `propext`, `Quot.sound` throughout.

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

§7 (equality reading) — which laws are *not* free from `Pledge/RE.hs`:

| Law         | §7 result                            | free? |
|-------------|--------------------------------------|-------|
| `L1`        | `re_L1_structural` (`lq Epsilon = id`)| syntactic |
| `R1`        | `re_R1_structural` (`revRE_involutive`)| syntactic |
| `S1 S2 C3`  | `re_{S1,S2,C3}_eq`                    | language eq; `normalize` recovers it |
| `S3 C1 C2`  | `re_{S3,C1,C2}_eq`                    | language eq only — `normalize` does not |
| `L3 R3`     | `re_{L3,R3}_eq`                       | + needs `IsLeftQuotient`/`IsRightQuotient` |
| `L2 R2`     | `re_L2_iff` / `re_L2_not_equality`   | **no** — needs `∃ w, lang divisor w` |
| `L4 R4`     | `re_L4_of_subsingleton` / `re_L4_not_equality` | **no** — needs divisor ⊨ ≤ 1 word |
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

-- ═══════════════════════════════════════════════════════════════════════════
-- § 7  Equality vs containment — which axioms hold "for free"
-- ═══════════════════════════════════════════════════════════════════════════

/-!
`ComposableAxioms` in `PledgeMonadLaws.lean` states every law as a Lean
**equality** `a = b` on the effect type, and the monad-law proofs `rw` with
them.  Containment (§2, §6) is therefore *not sufficient on its own*: the
Pledge monad laws hold for `eff = RE t` exactly as strongly as the 14 laws
hold **as equalities**.

This section pins that strength down, law by law, against the concrete
`instance Composable (RE t)` in `Pledge/RE.hs`
(`concatenation = Seq`, `conjunction = And`, `empty = Epsilon`,
`universe = top = Not Bot`, `leftQuotient = reLeftQuotient`,
`rightQuotient = reRightQuotient`).  Four tiers appear:

| tier | laws | meaning |
|------|------|---------|
| **structural**    | `L1`, `R1`                 | syntactic identity of the `RE` value the Haskell returns — holds for the derived `Eq` |
| **denotational**  | `S1 S2 S3 C1 C2 C3`        | both sides denote the same language (`RELEq`) but are different `RE` values; `normalize` closes the gap for `S1 S2 C3` only |
| **conditional**   | `L3 R3`                    | equality of languages, *provided* `reLeftQuotient`/`reRightQuotient` compute the true quotient (`IsLeftQuotient`/`IsRightQuotient`) |
| **fails in general** | `L2 R2 L4 R4`           | **not** equalities — only containments; each recovers equality only under an extra side condition, made explicit below |

The `Pledge`-level consequence: after a `>>=` the `pre`/`post`/`fut` fields of
`Pledge m (RE t) a` are only *language-equivalent* to the monad-law RHS, never
equal `RE` values (`S3 C1 C2`), and for `L2 R2 L4 R4` they agree only when the
postcondition acting as divisor is a *satisfiable, single* event trace — which
is what `Pledge/RE.hs` in fact produces (`post` starts at `ε` and grows by
`Seq` over concrete emitted events), but which is not forced by the types.
-/

namespace Eq7

set_option linter.unusedSectionVars false

variable {α : Type} [DecidableEq α]
open REComposableLaws

/-- Language equality — the reading of `=` under which the Pledge laws hold. -/
def RELEq (r1 r2 : RE α) : Prop := ∀ w, lang r1 w ↔ lang r2 w

-- ── Denotational tier: S1–S3, C1–C3 hold as full language equalities ───────

theorem re_S1_eq (r : RE α) : RELEq (.Seq .Eps r) r := by
  intro w; rw [lang_Seq, lang_Eps]
  exact ⟨S1 (lang r) w, fun hw => ⟨[], w, rfl, rfl, hw⟩⟩

theorem re_S2_eq (r : RE α) : RELEq (.Seq r .Eps) r := by
  intro w; rw [lang_Seq, lang_Eps]
  exact ⟨S2 (lang r) w, fun hw => ⟨w, [], (List.append_nil w).symm, hw, rfl⟩⟩

theorem re_S3_eq (r1 r2 r3 : RE α) :
    RELEq (.Seq (.Seq r1 r2) r3) (.Seq r1 (.Seq r2 r3)) := by
  intro w; simp only [lang_Seq]
  exact ⟨S3 (lang r1) (lang r2) (lang r3) w, S3' (lang r1) (lang r2) (lang r3) w⟩

theorem re_C1_eq (r1 r2 : RE α) : RELEq (.And r1 r2) (.And r2 r1) := by
  intro w; simp only [lang_And, andL]
  exact ⟨fun h => ⟨h.2, h.1⟩, fun h => ⟨h.2, h.1⟩⟩

theorem re_C2_eq (r1 r2 r3 : RE α) :
    RELEq (.And (.And r1 r2) r3) (.And r1 (.And r2 r3)) := by
  intro w; simp only [lang_And, andL]
  exact ⟨fun h => ⟨h.1.1, h.1.2, h.2⟩, fun h => ⟨⟨h.1, h.2.1⟩, h.2.2⟩⟩

theorem re_C3_eq (r : RE α) : RELEq (.And (.Not .Bot) r) r := by
  intro w; simp only [lang_And, lang_top, andL, topL, true_and]

/-- `S3 C1 C2` are *not* syntactic even after `normalize`: it neither
    reassociates `Seq` nor reorders `And`.  Illustration for `C1` / `S1`. -/
example : (RE.Seq RE.Eps (RE.Single (Event.Atom 0)) : RE Nat)
            ≠ RE.Single (Event.Atom 0) := by decide
example : (RE.And (RE.Single (Event.Atom 0)) (RE.Single (Event.Atom 1)) : RE Nat)
            ≠ RE.And (RE.Single (Event.Atom 1)) (RE.Single (Event.Atom 0)) := by decide

-- ── Structural tier: L1 (definitional), R1 (via `revRE` involution) ────────

/-- The first defining equation of `reLeftQuotient` (`Pledge/RE.hs`):
    `reLeftQuotient Epsilon r2 = r2`. -/
def LeftQuotientEpsId (lq : RE α → RE α → RE α) : Prop := ∀ r, lq .Eps r = r

/-- Model of `revRE` from `Pledge/RE.hs`. -/
def revRE : RE α → RE α
  | .Bot        => .Bot
  | .Eps        => .Eps
  | .Single e   => .Single e
  | .Seq r1 r2  => .Seq (revRE r2) (revRE r1)
  | .Or  r1 r2  => .Or  (revRE r1) (revRE r2)
  | .And r1 r2  => .And (revRE r1) (revRE r2)
  | .Star r     => .Star (revRE r)
  | .Not r      => .Not (revRE r)

/-- `reRightQuotient r1 r2 = revRE (reLeftQuotient (revRE r1) (revRE r2))`
    (`Pledge/RE.hs`). -/
def RightQuotientViaRev (rq lq : RE α → RE α → RE α) : Prop :=
  ∀ r1 r2, rq r1 r2 = revRE (lq (revRE r1) (revRE r2))

theorem revRE_involutive : ∀ r : RE α, revRE (revRE r) = r := by
  intro r
  induction r with
  | Bot => rfl
  | Eps => rfl
  | Single e => rfl
  | Seq r1 r2 ih1 ih2 => simp only [revRE, ih1, ih2]
  | Or  r1 r2 ih1 ih2 => simp only [revRE, ih1, ih2]
  | And r1 r2 ih1 ih2 => simp only [revRE, ih1, ih2]
  | Star r ih => simp only [revRE, ih]
  | Not  r ih => simp only [revRE, ih]

/-- **L1 is structural**: `reLeftQuotient Epsilon r = r` on the nose. -/
theorem re_L1_structural {lq : RE α → RE α → RE α}
    (heps : LeftQuotientEpsId lq) (r : RE α) : lq .Eps r = r := heps r

/-- **R1 is structural** modulo the (proved) involution `revRE ∘ revRE = id`. -/
theorem re_R1_structural {lq rq : RE α → RE α → RE α}
    (hrev : RightQuotientViaRev rq lq) (heps : LeftQuotientEpsId lq) (r : RE α) :
    rq .Eps r = r := by
  have h0 : revRE (.Eps : RE α) = .Eps := rfl
  rw [hrev .Eps r, h0, heps (revRE r), revRE_involutive]

-- ── Conditional tier: L3, R3 are language equalities given the quotient spec ─

theorem re_L3_eq {lq : RE α → RE α → RE α} (hlq : IsLeftQuotient lq)
    (ra rb rx : RE α) : RELEq (lq (.Seq ra rb) rx) (lq rb (lq ra rx)) := by
  intro w; simp only [hlq.lang_eq, lang_Seq]
  exact ⟨L3 (lang ra) (lang rb) (lang rx) w, L3' (lang ra) (lang rb) (lang rx) w⟩

theorem re_R3_eq {rq : RE α → RE α → RE α} (hrq : IsRightQuotient rq)
    (ra rb rx : RE α) : RELEq (rq ra (rq rb rx)) (rq (.Seq ra rb) rx) := by
  intro w; simp only [hrq.lang_eq, lang_Seq]
  exact ⟨R3 (lang ra) (lang rb) (lang rx) w, R3' (lang ra) (lang rb) (lang rx) w⟩

-- ── Failing tier: L2 / R2 need a satisfiable divisor ──────────────────────

/-- **L2 as an equality holds *iff* the divisor's language is non-empty.**
    `universe ∖ r = universe` says "some prefix in `L(r)` can be stripped";
    with `L(r) = ∅` there is none and the quotient collapses to `∅`. -/
theorem re_L2_iff {lq : RE α → RE α → RE α} (hlq : IsLeftQuotient lq) (rd : RE α) :
    RELEq (lq rd (.Not .Bot)) (.Not .Bot) ↔ ∃ w, lang rd w := by
  constructor
  · intro h
    have hx := (h []).mpr (by rw [lang_top]; trivial)
    rw [hlq.lang_eq] at hx
    obtain ⟨u, hu, _⟩ := hx
    exact ⟨u, hu⟩
  · rintro ⟨v, hv⟩ w
    rw [hlq.lang_eq, lang_top]
    exact ⟨fun _ => trivial, fun _ => ⟨v, hv, trivial⟩⟩

/-- Concrete failure of `L2` as an equality: `reLeftQuotient ∅ Σ* = ∅ ≠ Σ*`. -/
theorem re_L2_not_equality {lq : RE α → RE α → RE α} (hlq : IsLeftQuotient lq) :
    ¬ RELEq (lq (.Bot : RE α) (.Not .Bot)) (.Not .Bot) := by
  rw [re_L2_iff hlq]; rintro ⟨w, hw⟩; exact hw

/-- **R2** mirrors **L2**: equality iff the divisor is satisfiable. -/
theorem re_R2_iff {rq : RE α → RE α → RE α} (hrq : IsRightQuotient rq) (rd : RE α) :
    RELEq (rq rd (.Not .Bot)) (.Not .Bot) ↔ ∃ w, lang rd w := by
  constructor
  · intro h
    have hx := (h []).mpr (by rw [lang_top]; trivial)
    rw [hrq.lang_eq] at hx
    obtain ⟨u, hu, _⟩ := hx
    exact ⟨u, hu⟩
  · rintro ⟨v, hv⟩ w
    rw [hrq.lang_eq, lang_top]
    exact ⟨fun _ => trivial, fun _ => ⟨v, hv, trivial⟩⟩

theorem re_R2_not_equality {rq : RE α → RE α → RE α} (hrq : IsRightQuotient rq) :
    ¬ RELEq (rq (.Bot : RE α) (.Not .Bot)) (.Not .Bot) := by
  rw [re_R2_iff hrq]; rintro ⟨w, hw⟩; exact hw

-- ── Failing tier: L4 / R4 need a deterministic (≤ 1 word) divisor ──────────

/-- **L4 as an equality holds when the divisor denotes at most one word.**
    This is exactly the shape of a `Pledge` postcondition — an exact trace of
    emitted events — so `L4` is available *in practice* though not from the
    type. -/
theorem re_L4_of_subsingleton {lq : RE α → RE α → RE α} (hlq : IsLeftQuotient lq)
    (rd ra rb : RE α) (hdet : ∀ u u', lang rd u → lang rd u' → u = u') :
    RELEq (lq rd (.And ra rb)) (.And (lq rd ra) (lq rd rb)) := by
  intro w; simp only [hlq.lang_eq, lang_And, lqL, andL]
  constructor
  · rintro ⟨u, hu, ha, hb⟩; exact ⟨⟨u, hu, ha⟩, ⟨u, hu, hb⟩⟩
  · rintro ⟨⟨u, hu, ha⟩, ⟨u', hu', hb⟩⟩
    obtain rfl := hdet u u' hu hu'
    exact ⟨u, hu, ha, hb⟩

/-- Concrete failure of `L4` as an equality.  Divisor `{[0],[1]}` branches:
    `{[0],[1]} ∖ ({[0]} ∩ {[1]}) = ∅`, but
    `({[0],[1]} ∖ {[0]}) ∩ ({[0],[1]} ∖ {[1]}) = {ε} ∩ {ε} = {ε}`. -/
theorem re_L4_not_equality {lq : RE Nat → RE Nat → RE Nat} (hlq : IsLeftQuotient lq) :
    ¬ (∀ rd ra rb : RE Nat,
        RELEq (lq rd (.And ra rb)) (.And (lq rd ra) (lq rd rb))) := by
  intro h
  have hR : lang (RE.And
      (lq (.Or (.Single (.Atom 0)) (.Single (.Atom 1))) (.Single (.Atom 0)))
      (lq (.Or (.Single (.Atom 0)) (.Single (.Atom 1))) (.Single (.Atom 1)))) [] := by
    simp only [lang_And, hlq.lang_eq, andL, lqL]
    exact ⟨⟨[0], Or.inl ⟨0, rfl, rfl⟩, ⟨0, rfl, rfl⟩⟩,
           ⟨[1], Or.inr ⟨1, rfl, rfl⟩, ⟨1, rfl, rfl⟩⟩⟩
  have hL := (h _ _ _ []).mpr hR
  rw [hlq.lang_eq] at hL
  simp only [lang_And, andL, lqL] at hL
  obtain ⟨u, _, ha, hb⟩ := hL
  obtain ⟨x, hx, hx0⟩ := ha
  obtain ⟨y, hy, hy1⟩ := hb
  rw [hx] at hy; injection hy with hxy; subst hxy
  simp only [matchesEvent, decide_eq_true_eq] at hx0 hy1
  omega

/-- **R4** mirrors **L4**. -/
theorem re_R4_of_subsingleton {rq : RE α → RE α → RE α} (hrq : IsRightQuotient rq)
    (rd ra rb : RE α) (hdet : ∀ u u', lang rd u → lang rd u' → u = u') :
    RELEq (rq rd (.And ra rb)) (.And (rq rd ra) (rq rd rb)) := by
  intro w; simp only [hrq.lang_eq, lang_And, rqL, andL]
  constructor
  · rintro ⟨u, hu, ha, hb⟩; exact ⟨⟨u, hu, ha⟩, ⟨u, hu, hb⟩⟩
  · rintro ⟨⟨u, hu, ha⟩, ⟨u', hu', hb⟩⟩
    obtain rfl := hdet u u' hu hu'
    exact ⟨u, hu, ha, hb⟩

theorem re_R4_not_equality {rq : RE Nat → RE Nat → RE Nat} (hrq : IsRightQuotient rq) :
    ¬ (∀ rd ra rb : RE Nat,
        RELEq (rq rd (.And ra rb)) (.And (rq rd ra) (rq rd rb))) := by
  intro h
  have hR : lang (RE.And
      (rq (.Or (.Single (.Atom 0)) (.Single (.Atom 1))) (.Single (.Atom 0)))
      (rq (.Or (.Single (.Atom 0)) (.Single (.Atom 1))) (.Single (.Atom 1)))) [] := by
    simp only [lang_And, hrq.lang_eq, andL, rqL]
    exact ⟨⟨[0], Or.inl ⟨0, rfl, rfl⟩, ⟨0, rfl, rfl⟩⟩,
           ⟨[1], Or.inr ⟨1, rfl, rfl⟩, ⟨1, rfl, rfl⟩⟩⟩
  have hL := (h _ _ _ []).mpr hR
  rw [hrq.lang_eq] at hL
  simp only [lang_And, andL, rqL] at hL
  obtain ⟨u, _, ha, hb⟩ := hL
  obtain ⟨x, hx, hx0⟩ := ha
  obtain ⟨y, hy, hy1⟩ := hb
  rw [hx] at hy; injection hy with hxy; subst hxy
  simp only [matchesEvent, decide_eq_true_eq] at hx0 hy1
  omega

-- ── The honest bundle ─────────────────────────────────────────────────────

/-- The 14 `ComposableAxioms` laws, instantiated at `RE` and read with `=` as
    **language equality**, with the side conditions that `Pledge/RE.hs` cannot
    discharge from the types made explicit as hypotheses on `L2 R2 L4 R4`.
    `S1–S3 C1–C3` are unconditional; `L1 R1` are syntactic; `L3 R3` need only
    the quotient specification. -/
structure REEqualityLaws (lq rq : RE α → RE α → RE α) : Prop where
  S1 : ∀ r : RE α, RELEq (.Seq .Eps r) r
  S2 : ∀ r : RE α, RELEq (.Seq r .Eps) r
  S3 : ∀ r1 r2 r3 : RE α, RELEq (.Seq (.Seq r1 r2) r3) (.Seq r1 (.Seq r2 r3))
  C1 : ∀ r1 r2 : RE α, RELEq (.And r1 r2) (.And r2 r1)
  C2 : ∀ r1 r2 r3 : RE α, RELEq (.And (.And r1 r2) r3) (.And r1 (.And r2 r3))
  C3 : ∀ r : RE α, RELEq (.And (.Not .Bot) r) r
  L1 : ∀ r : RE α, lq .Eps r = r
  L2 : ∀ rd : RE α, (∃ w, lang rd w) → RELEq (lq rd (.Not .Bot)) (.Not .Bot)
  L3 : ∀ ra rb rx : RE α, RELEq (lq (.Seq ra rb) rx) (lq rb (lq ra rx))
  L4 : ∀ rd ra rb : RE α, (∀ u u', lang rd u → lang rd u' → u = u') →
         RELEq (lq rd (.And ra rb)) (.And (lq rd ra) (lq rd rb))
  R1 : ∀ r : RE α, rq .Eps r = r
  R2 : ∀ rd : RE α, (∃ w, lang rd w) → RELEq (rq rd (.Not .Bot)) (.Not .Bot)
  R3 : ∀ ra rb rx : RE α, RELEq (rq ra (rq rb rx)) (rq (.Seq ra rb) rx)
  R4 : ∀ rd ra rb : RE α, (∀ u u', lang rd u → lang rd u' → u = u') →
         RELEq (rq rd (.And ra rb)) (.And (rq rd ra) (rq rd rb))

/-- **Main result of §7.**  Given a correct quotient pair (`IsLeftQuotient` /
    `IsRightQuotient`) with the two defining equations `Pledge/RE.hs` supplies
    for free (`lq Epsilon = id`, `rq = revRE ∘ lq ∘ revRE`), `RE` satisfies all
    14 laws as equalities — under the stated side conditions for `L2 R2 L4 R4`,
    which are exactly the cases where the RE implementation does *not* give the
    axiom for free. -/
theorem re_equality_laws {lq rq : RE α → RE α → RE α}
    (hlq : IsLeftQuotient lq) (hrq : IsRightQuotient rq)
    (heps : LeftQuotientEpsId lq) (hrev : RightQuotientViaRev rq lq) :
    REEqualityLaws lq rq where
  S1 := re_S1_eq
  S2 := re_S2_eq
  S3 := re_S3_eq
  C1 := re_C1_eq
  C2 := re_C2_eq
  C3 := re_C3_eq
  L1 := heps
  L2 := fun rd hne => (re_L2_iff hlq rd).mpr hne
  L3 := re_L3_eq hlq
  L4 := fun rd ra rb hdet => re_L4_of_subsingleton hlq rd ra rb hdet
  R1 := fun r => re_R1_structural hrev heps r
  R2 := fun rd hne => (re_R2_iff hrq rd).mpr hne
  R3 := re_R3_eq hrq
  R4 := fun rd ra rb hdet => re_R4_of_subsingleton hrq rd ra rb hdet

end Eq7

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

**§7 — the equality reading demanded by `ComposableAxioms`.**  `PledgeMonadLaws`
`rw`s with each law as an `=`, so the Pledge monad laws hold for `eff = RE t`
only as strongly as the 14 laws hold *as equalities*.  Bundled as
`Eq7.re_equality_laws : REEqualityLaws lq rq`, with `=` read as language
equality `RELEq`:

| law | strength for `RE` | what is *not* free |
|-----|-------------------|--------------------|
| `L1` | **structural** (`lq Epsilon r = r`) | — |
| `R1` | **structural** modulo `revRE_involutive` (proved) | — |
| `S1 S2 C3` | language eq; also syntactic after `normalize` | `RE` values differ pre-`normalize` |
| `S3 C1 C2` | language eq only | `normalize` does *not* reassociate `Seq` / reorder `And`; monad-law RHS is only language-equivalent |
| `L3 R3` | language eq **given `IsLeftQuotient`/`IsRightQuotient`** | correctness of the `reLeftQuotient` worklist fixpoint + ACI cycle test + the `firstWith`/`Wildcard` finite-alphabet approximation for `Not` — not verified here |
| `L2 R2` | **fails**; equality iff `∃ w, lang divisor w` (`re_L2_iff`) | `reLeftQuotient ∅ Σ* = ∅ ≠ Σ*` (`re_L2_not_equality`): an unsatisfiable postcondition breaks left identity's `fut` |
| `L4 R4` | **fails**; only `⊆`. Equality iff the divisor denotes ≤ 1 word (`re_L4_of_subsingleton`) | `re_L4_not_equality`: a branching postcondition (`{[0],[1]}`) makes `(a⊓b)∖c` and `(a∖c)⊓(b∖c)` differ. Holds in practice only because `Pledge/RE.hs` builds `post` as an exact event trace. |

`Eq7.re_equality_laws` and the `re_*_not_equality` theorems depend only on
`propext`, `Quot.sound` — no `sorry`, no `Classical.choice`.
-/
