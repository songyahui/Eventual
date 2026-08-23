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
        (r1',     r2')    -> WAnd r1' r2'

    WStar r1 -> case wNormalize r1 of
        WBot   -> WEps sone    -- ∅* = ε
        WEps _ -> WEps sone    -- ε* = ε
        r1'    -> WStar r1'

    _ -> r

-- | Weighted left-quotient: the residual of @r2@ after consuming a prefix
-- described by @r1@.  Base case: @WEps _@ means the prefix is @ε@, so @r2@
-- is returned unchanged.
wLeftQuotient :: (Semiring w, Eq t) => WRE w t -> WRE w t -> WRE w t
wLeftQuotient (WEps _) r2 = r2
wLeftQuotient r1       r2 =
    let alph  = wAtoms r1 `union` wAtoms r2
        evts  = wFirstWith alph r1
        step e = wLeftQuotient (wNormalize (wDerivative e r1))
                                (wNormalize (wDerivative e r2))
    in foldr (WAdd . step) WBot evts

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

-- | @F[w](ev)@ — event @ev@ must eventually occur, weighted by @w@:
-- @Σ* · [w]ev@.  Use in @fut@ slots.
wFinally :: Semiring w => w -> Event t -> WRE w t
wFinally w ev = WSeq wTop (WSingle w ev)

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
