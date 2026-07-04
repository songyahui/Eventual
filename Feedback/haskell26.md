Haskell 2026 Paper #45 Reviews and Comments
===========================================================================
Paper #45 Effectful Computations with Future Conditions: A Monadic Approach
to Temporal Specification


Review #45A
===========================================================================

Overall merit
-------------
3. Weak accept

Reviewer expertise
------------------
3. Knowledgeable

Paper summary
-------------
This paper presents a lightweight Haskell library that augments pre/post-condition contracts with a third, data-dependent future condition `a -> eff` describing obligations the remainder of the program must discharge, which is propagated through monadic bind by subtracting the continuation's postcondition from the pending future via a Brzozowski-derivative-based residual. The same bind formula is instantiated across four domains, regex traces, regex + Presburger arithmetic, semiring-weighted regex, and separation-logic heap predicates, unified by a single Composable typeclass.

Comments for authors
--------------------
The paper has a neat key idea. As a program runs and emits events, one would naively record the trace and check membership at the end; the authors instead carry the obligation as a regular expression and, after each event (or, in `bind`, after each operation's postcondition), replace it with its Brzozowski derivative, so the residual expression captures exactly what remains to be discharged.

While the idea is pearl-worthy, the presentation barely fits the classic Functional Pearl format of a short, self-contained narrative built around a single trick (Please see - https://webspace.science.uu.nl/~swier004/pearls.html). It requires a non-trivial background in formal language theory (which the authors do supply), proves the monad laws (with an asterisk, discussed below), and packs in several largely independent ideas -- the GRE, weighted-semiring, and separation-logic instances. Given this structure and the number of case studies, I would recommend submitting it as a full research paper rather than a pearl.


Strengths
-------------
- The key idea of future obligations expressed as regular expressions, discharged event by event via Brzozowski derivatives, so that the residual expresses unmet obligations is unique and nice.
- The `Composable` typeclass subsuming four different obligation domains (RE, GRE, WRE, SL) under one bind formula is elegant
- The LTL_f embedding without DFA construction is useful in practice.
- The Haskell integration with no particularly advanced language extensions required is lightweight
- The breadth of case studies is decent

Weaknesses
-----------------
- Lack of a soundness theorem such as `residual normalises to success ⇒ every terminating execution satisfies the obligation.`
- The SL and WRE examples seem like a toy realisation of the idea and are not practically usable
- The distinction between specification and enforcement could be made more prominent earlier. The paper consistently uses "specification" throughout, but Section 11's explicit clarification that Pledge does not enforce obligations at runtime would benefit from being stated upfront.


Questions
--------------

1. A general question that I wondered while reading was - **What is the intended use of Pledge, concretely?** In every example, the annotations are written by the library author (tlsHandshake, takeMVar, malloc, alloc), so my question is about the consumer of a Pledge-annotated API. The consumer seems to differ across instances: the RE user is checking protocol-ordering obligations, whereas a WRE annotation like malloc a 0.9 encodes a quantitative claim that is an output of an analysis rather than a contract anyone writes by hand. For SL, are users meant to develop separation-logic specifications in Pledge at all, given that Section 7.6 admits the instance only records wands symbolically and never checks satisfiability or entailment? Or are SL and WRE intended not as usable instances but as evidence that the `Composable` abstraction generalises beyond traces?

2. What is the story with effects? The variable `eff` is always a pure data type (RE, GuardedRE, etc.) and the Section 3.5 and 8 primitives (takeMVar, tlsHandshake, malloc, alloc) are pure `Pledge` constructors that perform no IO. So when the paper claims Pledge "integrates transparently with the mtl stack," how does the pure specification side actually compose with a real `IO`, `StateT`, or `ReaderT` computation? 

3. Is `Pledge` truly a Monad in the usual sense (the laws hold on full `Pledge` values, all four fields), or only after redefining equality to ignore the `pre` field, which is the move Section 9 actually makes? If the latter, what justifies excluding `pre` from the equality the laws are checked against, given that pre is part of every `Pledge` record and is what the user inspects?

4. In Section 9, Left Identity proof, how is `post(f x) \ ¬∅` equal to `ε` from the subtraction definition given in Section 4? Can you please explain?



Review #45B
===========================================================================

Overall merit
-------------
1. Reject

Reviewer expertise
------------------
4. Expert

Paper summary
-------------
This paper presents a way of formalizing a temporal-logic style traces using the regular expression syntax as monads in Haskell. The key datatype is called `Pledge`, which includes a value, a precondition, a postcondition, and a future obligation. This paper shows that this `Pledge` datatype minus the precondition is a lawful monad. This paper also presents a few extensions to the `Pledge` datatype.

Comments for authors
--------------------
# Strengths

- Modeling Hoare-like triples as monads is not novel. However, modeling this style of extended regular expression as monads without relying on Haskell extensions is novel.
- It is not obvious that the four instances (RE, GRE, WRE, and SL) are instances of the framework, so showing that is also interesting.

# Weaknesses 

Although I am very familiar with all the technical content involved in this paper, I really struggled to understand this paper due to the writing and the technical content presented in this paper. More detailed comments:

- This paper models pre/postconditions using runtime data but does not explain why.
- I like the examples considered in the second paragraph of Section 1, but I had a lot of trouble understanding the rest of this section. I could not understand what the issue was that this paper tries to solve, or what approach it uses. In particular, temporal logic or specification has been studied in a lot of different contexts: in theorem provers, in model checkers, in types, etc. What you are trying to do is a runtime check, so you should motivate that and explain why.
  + Consider using an example to start this Section 1.
- Writing is particularly important in functional pearl, so I encourage the authors to pay close attention to the clarity of their writing.
- This paper seems to be based on two papers by Song et al. This paper cited those two papers, but *should* explicitly state the connection with those two papers in Section 1 due to the close connection.
- The `future` field of `Pledge` has type `a -> eff`, but I don't see why. I know that operations like `takeMVar` define `future` as `\r -> finally (... [Num r])`, but `future e` is always applied to `ret e` for any `e` (or `fe`). What's the point of making `future` a function then?
- It is not obvious to me why subtractions for GRE and SL are sound, especially when they are used in `future`. (I'm not saying that they are wrong; I have some vague intuition why they might be right, but the paper should explain and justify that.)
  + None of the examples related to GRE or SL help. Examples in Sections 5.4, 8.5, 8.7, 8.8, and 8.9 all use trivial `future` conditions `universe` or `Top`. This is very odd given the paper is about future conditions.
  + I'm also quite confused by some of the examples. For example, the first bad example in Section 8.6 adds a `free 0 1` to the good example. How could the program that frees only one address be bad, while the program that frees nothing be good?
  + Using magic wand for subtraction feels quite counterintuitive to me. Is it based on the adjoint property `P * Q |-- R   <->   P |-- Q −∗ R` or something else? I can't be sure, especially since there is no example with how it works for `future`. Or did I miss anything?
- The related work section cites works that are not closely related (like QuickCheck's precondition), but misses works on modeling specifications as monads, in particular works on the Dijkstra monad, etc.

Minor comments:
- "No single pre/post pair can express these properties..." This claim is too strong. There are many flavors of pre/post-conditions for this purpose.
- "Intersection r1 /\ r2 (And) is derived..." But this doesn't match the code you just presented. You had "And", which was not derived.
- "allow complement to be propagated..." Extra space in the beginning.
- "...purely algebraically, without building a DFA." But this also means your formalization only has the syntax of regular expression, not the semantics. You cannot justify or prove soundness, which should be acknowledged as a limitation.
- "...purely algebraically, without building a DFA." Explain that DFA is a deterministic finite automaton.
- "The normaliser additionally applies..." What normaliser? You also normalise "not empty" to the same thing?
- "The `firstWith` function returns... Naively writing `first`..." Is it `firstWithin` or `first`? This paragraph is also very hard to understand.
- "Illustrative cases": this paragraph is very hard to understand.
- Naming the value of `Pledge` as `ret` is quite confusing as it conflicts with monadic `ret`.
- You use `\` in some places for subtraction and `\\` in some other places. Please be consistent.
- Equations 3 and 4: What is $e_2$? It is a free variable in both equations. (I know it is 
- Section 3.4: Why do you need this? Can't you derive `<*>` from `>>=`?
- Section 5.3: It seems that `PExpr` is the key to your definitions in Section 5, but you never showed it.
- What is `ValAt`?
- Section 9: I think you should be more explicit about the limitation of `pre` upfront (I know you explained this later; but explain that in the beginning!).
- Section 10, "Typestate": What is "annotation-inpsection time"? Isn't that just part of runtime?



Review #45C
===========================================================================

Overall merit
-------------
3. Weak accept

Reviewer expertise
------------------
2. Some familiarity

Paper summary
-------------
Pledge adds future conditions to the usual pre/post contract model: obligations
that propagate forward until discharged. The Pledge monad carries `pre`, `post`,
and a data-dependent `future :: a -> eff`. Bind discharges the first
computation's future against the continuation's postcondition by subtraction,
and conjoins the residual with the continuation's own future. The whole thing is
parameterised over a five-operation `Composable` algebra (concatenation,
conjunction, subtraction, empty, universe), with four instances: extended REs
with complement (subtraction is the Brzozowski derivative, LTL_f embeds
directly), Presburger-guarded REs (discharged by Z3), semiring-weighted REs
(probabilistic and min-cost), and separation-logic heap predicates (subtraction
is the magic wand). The unifying point: derivative subtraction is obligation
propagation, and a residual that isn't `universe` names exactly the unmet
obligation. Monad laws are checked for the (ret, post, future) triple. Ten
worked examples span the instances, including TLS handshake/bye, MVar
discipline, locks, DB transactions, a bounded counter, task scheduling, and
probabilistic and heap memory. It is well presented, and the single algebra covering four very
different obligation kinds (traces, arithmetic, weights, heaps) is the elegant
part.

Comments for authors
--------------------
## Strengths

- The core idea is elegant. One bind formula, and "obligation propagation"
  falls out as Brzozowski-derivative subtraction. The obligation check is uniform across all
  four instances: after composing, you read off the residual, and anything that
  isn't `universe` names exactly what is still unmet.
- The four instances landing on the same five-operation algebra is satisfying. Subtraction is a derivative for REs, the magic wand for
  separation logic, semiring-weighted for WRE. Same bind, four very different
  obligation kinds.
- Well presented. It builds from pre/post to future conditions to the algebra to
  the instances, with worked examples the whole way. Easy to follow.
- Lots of good related work, well placed: Liquid Haskell,
  Hoare-triple indexed monad, indexed and graded monads, session types,
  typestate, linear types, ResourceT, ProveNFix. The positioning is careful and
  fair.
- Honest about the limits. The SL instance records obligations symbolically and
  runs no solver; Pledge is specification, not runtime enforcement; `pre`
  deviates from left-identity on a precondition violation and is handled as a
  separate layer. All said plainly.

## Weaknesses

- Specification, not enforcement. The check runs on the spec value before
  execution, so an async exception can still violate the obligation at runtime.
  Acknowledged in Sec. 11; say up front whether this catches the bug or only
  describes it.
- The four instances are not on equal footing. Three discharge obligations (RE
  by derivatives, GRE by Z3, WRE by semiring weight); SL only records the wand
  symbolically, with no solver. So "works out for all four" is partly promissory
  for SL. Acknowledged, but it undercuts the headline a little.
- `pre` is the one caveat to the clean monad story. It deviates from
  left-identity when a precondition violation is detected, so it sits outside the
  (ret, post, future) triple the laws are proved for and is carried as a separate
  writer-like annotation (Sec. 9). The "it's a lawful monad" claim holds only for
  that triple; `pre` is a separate contract-checking layer.
- It runs long for a pearl (26 pages, four instances, ten examples). The content
  earns most of it, but it could be tighter: moving GRE or WRE to an appendix
  would keep the main line leaner. Minor.

## Questions

- Would the pearl be sharper with two instances and the rest in an appendix? RE
  plus SL already shows the range; GRE and WRE feel like they are there for
  completeness.
- How much of the elegance survives once the SL instance wires in a real solver
  (Smallfoot, Z3)? Does subtraction-as-wand still compose this cleanly when you
  actually discharge?
- Could `pre` be pulled out into a separate static pass (you float this in Sec. 11)
  so the monad is clean and the contract check sits beside it? Would that be
  more elegant?
- What would integration with real code look like? The MVar example uses `type
  MVar = Int` and Pledge's own `takeMVar`/`putMVar`, not `Control.Concurrent.MVar`.
  To enforce MVar discipline in a real program, would you rewrite it in the
  Pledge monad, keep the Sec. 11 shadow spec beside it, or write an interpreter from
  a Pledge term to real IO? What keeps the spec faithful to the code that runs?
- Any handle on liveness or omega-regular obligations? `finally` here is
  finite-trace (the event must occur within the observed trace); true
  "eventually" over infinite runs seems out of scope. A sentence would help.



Comment @A1 by Reviewer B
---------------------------------------------------------------------------
Dear authors,

We regret to inform you that the PC has decided to reject this paper after discussion. Please refer to the reviews for PC's detailed feedback.

Some top feedback from PC discussion:
- Clarity in writing is especially important for a functional pearl, so please work on improving that. See reviews for detailed feedback on writing.
- Please consider and clarify the intended use of `Pledge`.
- The paper should clarify the close connection to Song et al.'s paper "Specifying and Verifying Future Conditions" and "ProveNFix: Temporal Property-Guided Program Repair" early on (for example, in Section 1).
- The paper should discuss the limitation that `pre` does not obey monad laws early on.
- Given that the title of the paper is "Effectful Computations with Future Conditions," there should be a discussion on how the proposed framework work with effects.
  + Related to this point, the reviewers are confused about the type of `post` and `future`. Why make `future` a function `a -> eff` when you always apply it to the `ret` field of the same `Pledge` record? If it's to deal with effects with unknown returned value (like IO operations that returns a file descriptor), why don't you need `post` to be a function? If you actually need to apply `post` or `future` to arguments that are not `ret`, how would you define `>>=` then?
- Although it's nice to have four instances of `Composable`, there should be some validation why these work as intended (for example, property-based testing, comprehensive case studies, etc.).
  + The paper should have a concrete soundness theorem that states why residual normalizes to success ⇒ every terminating execution satisfies the obligation.
- Many of the examples shown in this paper have trivial `future` conditions. Please consider using examples that actually need interesting `future` conditions, since this paper's central thesis is that pre-/post-conditions are not sufficient.