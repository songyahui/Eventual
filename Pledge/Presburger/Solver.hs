module Pledge.Presburger.Solver
    ( -- * Solver result
      SolverResult(..)
    , checkPPred
    , isValidUnderHeapInvariant
    ) where

import Control.Exception (try, SomeException)
import Data.List (nub, intercalate)
import qualified Data.Map.Strict as Map
import Data.SBV hiding (Unsatisfiable)
import Pledge.Event (Addr)
import Pledge.Presburger

-- ── Result type ───────────────────────────────────────────────────────────────

-- | A witness heap maps each address to its concrete integer value.
data SolverResult
    = Satisfied (Map.Map Addr Integer)  -- ^ SAT: a concrete witness heap
    | Unsatisfiable                     -- ^ UNSAT: no heap satisfies the constraints
    | SolverUnknown String              -- ^ solver unavailable or timed out
    deriving (Eq)

instance Show SolverResult where
    show (Satisfied h)
        | Map.null h = "SAT  (no heap variables)"
        | otherwise  = "SAT  heap = { "
                    ++ intercalate ", " [ "h[" ++ show a ++ "] := " ++ show v
                                        | (a, v) <- Map.toList h ]
                    ++ " }"
    show Unsatisfiable       = "UNSAT"
    show (SolverUnknown msg) = "UNKNOWN (" ++ msg ++ ")"

-- ── Address / variable collection ─────────────────────────────────────────────

addrsInPExpr :: PExpr -> [Addr]
addrsInPExpr (Lit _)     = []
addrsInPExpr (ValAt a)   = [a]
addrsInPExpr (Var _)     = []
addrsInPExpr (Add e1 e2) = addrsInPExpr e1 ++ addrsInPExpr e2
addrsInPExpr (Mul _ e)   = addrsInPExpr e

addrsInPPred :: PPred -> [Addr]
addrsInPPred PTrue        = []
addrsInPPred PFalse       = []
addrsInPPred (PLt  e1 e2) = addrsInPExpr e1 ++ addrsInPExpr e2
addrsInPPred (PLe  e1 e2) = addrsInPExpr e1 ++ addrsInPExpr e2
addrsInPPred (PEq  e1 e2) = addrsInPExpr e1 ++ addrsInPExpr e2
addrsInPPred (PGt  e1 e2) = addrsInPExpr e1 ++ addrsInPExpr e2
addrsInPPred (PGe  e1 e2) = addrsInPExpr e1 ++ addrsInPExpr e2
addrsInPPred (PNot p)     = addrsInPPred p
addrsInPPred (PAnd p q)   = addrsInPPred p ++ addrsInPPred q

-- | Free (non-heap) variable names mentioned in a 'PExpr' / 'PPred'.
varsInPExpr :: PExpr -> [String]
varsInPExpr (Lit _)     = []
varsInPExpr (ValAt _)   = []
varsInPExpr (Var x)     = [x]
varsInPExpr (Add e1 e2) = varsInPExpr e1 ++ varsInPExpr e2
varsInPExpr (Mul _ e)   = varsInPExpr e

varsInPPred :: PPred -> [String]
varsInPPred PTrue        = []
varsInPPred PFalse       = []
varsInPPred (PLt  e1 e2) = varsInPExpr e1 ++ varsInPExpr e2
varsInPPred (PLe  e1 e2) = varsInPExpr e1 ++ varsInPExpr e2
varsInPPred (PEq  e1 e2) = varsInPExpr e1 ++ varsInPExpr e2
varsInPPred (PGt  e1 e2) = varsInPExpr e1 ++ varsInPExpr e2
varsInPPred (PGe  e1 e2) = varsInPExpr e1 ++ varsInPExpr e2
varsInPPred (PNot p)     = varsInPPred p
varsInPPred (PAnd p q)   = varsInPPred p ++ varsInPPred q

-- ── SBV translation ───────────────────────────────────────────────────────────

pexprToSBV :: Map.Map Addr SInteger -> Map.Map String SInteger -> PExpr -> SInteger
pexprToSBV _  _  (Lit n)     = fromIntegral n
pexprToSBV hv _  (ValAt a)   = hv Map.! a
pexprToSBV _  vv (Var x)     = vv Map.! x
pexprToSBV hv vv (Add e1 e2) = pexprToSBV hv vv e1 + pexprToSBV hv vv e2
pexprToSBV hv vv (Mul k e)   = fromIntegral k * pexprToSBV hv vv e

ppredToSBV :: Map.Map Addr SInteger -> Map.Map String SInteger -> PPred -> SBool
ppredToSBV _  _  PTrue        = sTrue
ppredToSBV _  _  PFalse       = sFalse
ppredToSBV hv vv (PLt  e1 e2) = pexprToSBV hv vv e1 .<  pexprToSBV hv vv e2
ppredToSBV hv vv (PLe  e1 e2) = pexprToSBV hv vv e1 .<= pexprToSBV hv vv e2
ppredToSBV hv vv (PEq  e1 e2) = pexprToSBV hv vv e1 .== pexprToSBV hv vv e2
ppredToSBV hv vv (PGt  e1 e2) = pexprToSBV hv vv e1 .>  pexprToSBV hv vv e2
ppredToSBV hv vv (PGe  e1 e2) = pexprToSBV hv vv e1 .>= pexprToSBV hv vv e2
ppredToSBV hv vv (PNot p)     = sNot (ppredToSBV hv vv p)
ppredToSBV hv vv (PAnd p q)   = ppredToSBV hv vv p .&& ppredToSBV hv vv q

-- ── Solver ────────────────────────────────────────────────────────────────────
-- All ValAt references become unbounded integer variables; so does every
-- free 'Var'. Only the heap side is reported back in the witness — a 'Var'
-- is existentially quantified for satisfiability but is not part of the heap.

-- | Check satisfiability of a 'PPred' using an SMT solver.
-- Returns a concrete witness heap on SAT, 'Unsatisfiable' on UNSAT, or
-- 'SolverUnknown' if the solver is unavailable or times out.
checkPPred :: PPred -> IO SolverResult
checkPPred p = do
    let addrs  = nub (addrsInPPred p)
        names  = nub (varsInPPred p)
        varFor = ("h" ++) . show
    eResult <- try $ sat $ do
        hvars <- mapM (sInteger . varFor) addrs
        vvars <- mapM sInteger names
        let hv = Map.fromList (zip addrs hvars)
            vv = Map.fromList (zip names vvars)
        return (ppredToSBV hv vv p)
    case eResult of
        Left  (e :: SomeException) -> return (SolverUnknown (show e))
        Right result ->
            if modelExists result
                then do
                    let vals = [ (a, v)
                               | a <- addrs
                               , Just v <- [getModelValue (varFor a) result] ]
                    return (Satisfied (Map.fromList vals))
                else return Unsatisfiable

-- | Is a 'PPred' valid for every heap satisfying the standard domain
-- invariant that heap values are non-negative — i.e.\ does
-- @(⋀_a h[a] ≥ 0) ⟹ p@ hold?  Plain Presburger validity would reject e.g.
-- @h[a] = 0 ∨ h[a] > 0@ (it fails for @h[a] = -1@), even though every
-- reachable heap in this library only ever holds non-negative values
-- (see the 'GuardedRE' module header). Checked via Z3 by testing that the
-- invariant conjoined with @¬p@ is unsatisfiable.
isValidUnderHeapInvariant :: PPred -> IO Bool
isValidUnderHeapInvariant p = do
    let axiom = foldr (PAnd . nonNeg) PTrue (nub (addrsInPPred p))
        nonNeg a = PGe (ValAt a) (Lit 0)
    result <- checkPPred (PAnd axiom (PNot p))
    return (result == Unsatisfiable)
