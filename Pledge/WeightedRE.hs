module Pledge.WeightedRE
    ( -- * Weighted regular expressions
      WRE(..)
      -- * Semantics
    , wNullable
      -- * Alphabet
    , wAtoms
    , wFirstWith
    , wFirst
      -- * Derivatives
    , wDerivative
      -- * Normalization
    , wNormalize
      -- * Quotient
    , wLeftQuotient
    , wRightQuotient
      -- * Smart constructors
    , wTop
    , wFinally
    , wGlobally
    , wPreviously
    ) where

import Prelude hiding ((<>))
import Data.List (union, nub)
import Pledge.Core
import Pledge.Presburger (Event(..), subsumesEvent)
import Pledge.Semiring

-- | A regular expression whose transitions carry weights from a 'Semiring' @w@.
--
-- The language of a @WRE w t@ is a function @Σ* → w@ (words to weights).
-- Key operations:
--
-- * 'wNullable' @r@     — weight assigned to @ε@ by @r@.
-- * 'wDerivative' @e r@ — @WRE@ for all continuations after event @e@.
--
-- The Boolean special case @WRE Bool@ recovers plain 'RE' exactly:
-- @WBot ↔ Bot@, @WEps True ↔ Epsilon@, @WSeq ↔ Seq@, @WAdd ↔ Or@, etc.
data WRE w t
    = WBot                       -- ^ weight-0 everywhere (empty language)
    | WEps w                     -- ^ @ε@ accepted with weight @w@
    | WSingle w (Event t)        -- ^ single event accepted with weight @w@
    | WSeq  (WRE w t) (WRE w t) -- ^ sequential composition (@⊗@ on languages)
    | WAdd  (WRE w t) (WRE w t) -- ^ weighted choice (@⊕@ on languages)
    | WAnd  (WRE w t) (WRE w t) -- ^ pointwise conjunction (pointwise @⊗@)
    | WStar (WRE w t)            -- ^ Kleene star
    deriving (Eq)

instance (Semiring w, Show t) => Show (WRE w t) where
    show WBot                           = "∅"
    show (WEps w)     | w == sone       = "ε"
                      | otherwise       = "[" ++ show w ++ "]ε"
    show (WSingle w Wildcard)
                      | w == sone       = "Σ"
                      | otherwise       = "[" ++ show w ++ "]Σ"
    show (WSingle w e)| w == sone       = show e
                      | otherwise       = "[" ++ show w ++ "]" ++ show e
    -- recognise common patterns for nicer display
    -- wFinally: Σ* · [w]ev · Σ*
    show (WSeq (WStar (WSingle w1 Wildcard))
               (WSeq (WSingle w2 ev) (WStar (WSingle w3 Wildcard))))
        | w1 == sone && w3 == sone && w2 == sone = "F(" ++ show ev ++ ")"
        | w1 == sone && w3 == sone   = "F[" ++ show w2 ++ "](" ++ show ev ++ ")"
    show (WSeq (WStar (WSingle w1 Wildcard)) (WSingle w2 ev))
        | w1 == sone && w2 == sone      = "F(" ++ show ev ++ ")"
        | otherwise                     = "F[" ++ show w2 ++ "](" ++ show ev ++ ")"
    show (WStar (WSingle w ev))
        | w == sone                     = "G(" ++ show ev ++ ")"
    show r = go r
      where
        go (WSeq r1 r2) = show r1 ++ " · " ++ show r2
        go (WAdd r1 r2) = "(" ++ show r1 ++ ") ⊕ (" ++ show r2 ++ ")"
        go (WAnd r1 r2) = "(" ++ show r1 ++ ") ∧ (" ++ show r2 ++ ")"
        go (WStar r1)   = "(" ++ show r1 ++ ")*"
        go r1           = show r1

-- | Weight assigned to the empty word @ε@ by a 'WRE'.
-- Generalises @nullable :: RE -> Bool@: in the Boolean semiring,
-- @wNullable r == sone@ iff @nullable r == True@.
wNullable :: Semiring w => WRE w t -> w
wNullable WBot          = szero
wNullable (WEps w)      = w
wNullable (WSingle _ _) = szero
wNullable (WSeq r1 r2)  = smul (wNullable r1) (wNullable r2)
wNullable (WAdd r1 r2)  = sadd (wNullable r1) (wNullable r2)
wNullable (WAnd r1 r2)  = smul (wNullable r1) (wNullable r2)
wNullable (WStar _)     = sone   -- ε ∈ L(r*) with unit weight for every semiring

-- ── Alphabet ──────────────────────────────────────────────────────────────────

-- | Collect all concrete (non-'Wildcard') events mentioned in a 'WRE'.
-- Forms the effective alphabet for complement unfolding in 'wFirstWith'.
wAtoms :: Eq t => WRE w t -> [Event t]
wAtoms WBot               = []
wAtoms (WEps _)           = []
wAtoms (WSingle _ Wildcard) = []
wAtoms (WSingle _ e)      = [e]
wAtoms (WSeq  r1 r2)      = wAtoms r1 `union` wAtoms r2
wAtoms (WAdd  r1 r2)      = wAtoms r1 `union` wAtoms r2
wAtoms (WAnd  r1 r2)      = wAtoms r1 `union` wAtoms r2
wAtoms (WStar r)          = wAtoms r

-- | Events from @alph@ that can begin a word with non-zero weight in a 'WRE'.
wFirstWith :: (Semiring w, Eq t) => [Event t] -> WRE w t -> [Event t]
wFirstWith _    WBot                    = []
wFirstWith _    (WEps _)               = []
wFirstWith alph (WSingle _ Wildcard)   = alph
wFirstWith _    (WSingle _ e)          = [e]
wFirstWith alph (WSeq r1 r2)
    | wNullable r1 /= szero            = wFirstWith alph r1 `union` wFirstWith alph r2
    | otherwise                        = wFirstWith alph r1
wFirstWith alph (WAdd r1 r2)           = wFirstWith alph r1 `union` wFirstWith alph r2
wFirstWith alph (WAnd r1 r2)           = [ e | e <- wFirstWith alph r1
                                             , e `elem` wFirstWith alph r2 ]
wFirstWith alph (WStar r)              = wFirstWith alph r

-- | Convenience wrapper: uses the events in @r@ itself as the alphabet.
wFirst :: (Semiring w, Eq t) => WRE w t -> [Event t]
wFirst r = wFirstWith (wAtoms r) r

-- | Weighted Brzozowski derivative: the 'WRE' for all continuations after event @e@.
-- When the left operand of 'WSeq' is nullable, both branches contribute,
-- weighted by @wNullable r1@.
wDerivative :: (Semiring w, Eq t) => Event t -> WRE w t -> WRE w t
wDerivative _ WBot             = WBot
wDerivative _ (WEps _)         = WBot
wDerivative e (WSingle w p)
    | subsumesEvent e p        = WEps w
    | otherwise                = WBot
wDerivative e (WSeq r1 r2)
    | wNullable r1 /= szero    =
        WAdd (WSeq (wDerivative e r1) r2)
             (WSeq (WEps (wNullable r1)) (wDerivative e r2))
    | otherwise                = WSeq (wDerivative e r1) r2
wDerivative e (WAdd r1 r2)     = WAdd (wDerivative e r1) (wDerivative e r2)
wDerivative e (WAnd r1 r2)     = WAnd (wDerivative e r1) (wDerivative e r2)
wDerivative e (WStar r)        = WSeq (wDerivative e r) (WStar r)

-- | Structural simplification of a 'WRE'.
-- Applies identities that hold for every semiring:
-- @WBot@ absorption, @WEps sone@ identity for 'WSeq', @WBot@ identity for 'WAdd',
-- and @∅* = ε@, @ε* = ε@.
wNormalize :: Semiring w => WRE w t -> WRE w t
wNormalize r = case r of
    WSeq r1 r2 -> case (wNormalize r1, wNormalize r2) of
        (WBot,    _)               -> WBot
        (_,       WBot)            -> WBot
        (WEps w1, WEps w2)         -> WEps (smul w1 w2)
        (WEps w,  r2') | w == sone -> r2'
        (r1', WEps w)  | w == sone -> r1'
        (r1', r2')                 -> WSeq r1' r2'

    WAdd r1 r2 -> case (wNormalize r1, wNormalize r2) of
        (WBot, r')    -> r'
        (r',   WBot)  -> r'
        (r1',  r2')   -> WAdd r1' r2'

    WAnd r1 r2 -> case (wNormalize r1, wNormalize r2) of
        (WBot,    _)      -> WBot
        (_,       WBot)   -> WBot
        (WEps w1, WEps w2)-> WEps (smul w1 w2)
        -- 'wTop' is the identity for 'WAnd', exactly as Σ* is for RE's 'And'.
        -- It assigns @sone@ to every word (@sone ⊗ … ⊗ sone = sone@), and
        -- @sone@ is the identity for @⊗@, so @wTop ⊗ r = r@ pointwise.
        -- Without this rule a discharged residual keeps accumulating
        -- @WAnd _ wTop@ layers and never reduces to the identity.
        (r1',     r2')
            | isWTop r1'  -> r2'
            | isWTop r2'  -> r1'
        (r1',     r2')    -> WAnd r1' r2'

    WStar r1 -> case wNormalize r1 of
        WBot   -> WEps sone    -- ∅* = ε
        WEps _ -> WEps sone    -- ε* = ε
        r1'    -> WStar r1'

    _ -> r

-- | Weighted left-quotient: the residual of @r2@ after consuming a prefix
-- described by @r1@.  As a formal power series,
--
-- @
--   (r1 \\ r2)(w)  =  ⊕_u  r1(u) ⊗ r2(u·w)
-- @
--
-- which unfolds into the recurrence
--
-- @
--   r1 \\ r2  =  (ν(r1) ⊗ r2)  ⊕  ⊕_e (∂_e r1) \\ (∂_e r2)
-- @
--
-- The first summand is the @u = ε@ term, weighted by @r1@'s own ε-weight:
-- it is /not/ simply @r2@, since a prefix language accepting @ε@ with weight
-- @w@ contributes @w ⊗ r2@ and dropping @w@ silently loses it.
--
-- Solved by the same worklist traversal as 'Pledge.RE.reLeftQuotient': walk
-- the reachable pairs @(∂_w r1, ∂_w r2)@, accumulate the nullable
-- contribution at each, and drop a pair already seen.
--
-- __Termination.__ Unlike the Boolean case this is /not/ guaranteed in
-- general.  Cycle detection here uses plain structural equality, because ACI
-- normalisation would be unsound: neither @⊕@ nor @⊗@ is idempotent in an
-- arbitrary semiring (@'Prob' 0.5 ⊕ 'Prob' 0.5 = 'Prob' 1.0@).  Worse, a
-- weighted star has infinitely many distinct derivatives whenever its weight
-- does not stabilise under @⊗@ --- @([0.9]a)*@ generates
-- @0.9, 0.81, 0.729, …@.  Termination therefore holds exactly when the
-- divisor has finitely many distinct derivative terms: for unit weights
-- (@'wTop'@, @'wGlobally' 'sone'@, and the whole Boolean semiring, where
-- @∂_e r = r@ holds on the nose) it does; for a divisor carrying a
-- non-unit star weight it may not.  Divisors in practice are postconditions,
-- which are unit-weight event sequences, so this is not hit --- but it is a
-- real restriction and not merely a missing optimisation.
wLeftQuotient :: (Semiring w, Eq t) => WRE w t -> WRE w t -> WRE w t
wLeftQuotient r1 r2 = wNormalize (go [(r1, r2)] [] WBot)
  where
    go []             _    acc = acc
    go ((p, q):queue) seen acc
        | (p, q) `elem` seen = go queue seen acc
        | otherwise          = go (nexts ++ queue) ((p, q) : seen) acc'
      where
        -- u = ε contributes ν(p) ⊗ q.
        acc' | wNullable p /= szero = WAdd (WSeq (WEps (wNullable p)) q) acc
             | otherwise            = acc
        -- 'Wildcard' represents Σ minus the named atoms; without it a pair
        -- naming no concrete event (wTop, WEps, WBot) has no successors and
        -- the result collapses to WBot.
        alph  = Wildcard : (wAtoms p `union` wAtoms q)
        nexts = [ (wNormalize (wDerivative e p), wNormalize (wDerivative e q))
                | e <- wFirstWith alph p
                ]

-- Reverse a WRE: wRev(r) accepts exactly {w^R | w ∈ L(r)} with the same weights.
wRev :: WRE w t -> WRE w t
wRev WBot             = WBot
wRev (WEps w)         = WEps w
wRev (WSingle w e)    = WSingle w e
wRev (WSeq  r1 r2)    = WSeq  (wRev r2) (wRev r1)
wRev (WAdd  r1 r2)    = WAdd  (wRev r1) (wRev r2)
wRev (WAnd  r1 r2)    = WAnd  (wRev r1) (wRev r2)
wRev (WStar r)        = WStar (wRev r)

-- | Weighted right-quotient: the residual of @r2@ with @r1@ stripped from
-- the right.  Computed via reversal: @r2 ∕ r1 = wRev(wRev(r2) ∖ wRev(r1))@.
wRightQuotient :: (Semiring w, Eq t) => WRE w t -> WRE w t -> WRE w t
wRightQuotient r1 r2 = wRev (wLeftQuotient (wRev r1) (wRev r2))

-- ── Smart constructors ────────────────────────────────────────────────────────

-- | @Σ*@ — universal language with unit weight (@sone@) on every transition.
wTop :: Semiring w => WRE w t
wTop = WStar (WSingle sone Wildcard)

-- | Is this term 'wTop'?  Used by 'wNormalize' to apply the 'WAnd' identity.
isWTop :: Semiring w => WRE w t -> Bool
isWTop (WStar (WSingle w Wildcard)) = w == sone
isWTop _                            = False

-- | @F[w](ev)@ — event @ev@ must eventually occur, weighted by @w@:
-- @Σ* · [w]ev · Σ*@.  Use in @fut@ slots.
--
-- The trailing @Σ*@ is essential and mirrors 'Pledge.RE.finally'.  Without
-- it the obligation reads ``the trace /ends with/ @ev@'' rather than
-- ``@ev@ occurs somewhere'', so an obligation discharged anywhere but at the
-- very last event is reported as unmet: in @submit 1; complete 1; submit 2;
-- complete 2@ task 1's obligation would survive, because the trace ends with
-- @complete(2)@.
wFinally :: Semiring w => w -> Event t -> WRE w t
wFinally w ev = WSeq wTop (WSeq (WSingle w ev) wTop)

-- | @G[w](ev)@ — every step must be @ev@, each with weight @w@: @([w]ev)*@.
wGlobally :: Semiring w => w -> Event t -> WRE w t
wGlobally w ev = WStar (WSingle w ev)

-- | Past-facing alias for 'wFinally'.
-- Use in @pre@ slots to assert @ev@ occurred somewhere in the preceding trace.
wPreviously :: Semiring w => w -> Event t -> WRE w t
wPreviously w ev = WSeq wTop (WSeq (WSingle w ev) wTop)

-- ── Composable instance ───────────────────────────────────────────────────────
-- WRE w lifts the Composable algebra to the weighted setting.
-- This makes Pledge (WRE Prob) and Pledge (WRE Tropical) work out of the box.

instance (Semiring w, Eq t) => Composable (WRE w t) where
    concatenation r1 r2 = wNormalize (WSeq r1 r2)
    conjunction   r1 r2 = wNormalize (WAnd r1 r2)
    empty               = WEps sone
    universe            = wTop
    leftQuotient        = wLeftQuotient
    rightQuotient       = wRightQuotient
