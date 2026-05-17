module Pledge.Solver
    ( SolverResult(..)
    , checkPPred
    ) where

import Control.Exception (try, SomeException)
import Data.List (nub, intercalate)
import qualified Data.Map.Strict as Map
import Data.SBV hiding (Unsatisfiable)
import Pledge.Utils

-- ── Result type ───────────────────────────────────────────────────────────────

-- A witness heap maps each address to its concrete integer value.
data SolverResult
    = Satisfied (Map.Map Addr Integer)  -- SAT: a concrete witness heap
    | Unsatisfiable                     -- UNSAT: no heap satisfies the constraints
    | SolverUnknown String              -- solver unavailable or timeout
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

-- ── Address collection ────────────────────────────────────────────────────────

addrsInPExpr :: PExpr -> [Addr]
addrsInPExpr (Lit _)     = []
addrsInPExpr (ValAt a)   = [a]
addrsInPExpr (Add e1 e2) = addrsInPExpr e1 ++ addrsInPExpr e2
addrsInPExpr (Mul _ e)   = addrsInPExpr e

addrsInPPred :: PPred -> [Addr]
addrsInPPred PTrue        = []
addrsInPPred (PLt  e1 e2) = addrsInPExpr e1 ++ addrsInPExpr e2
addrsInPPred (PLe  e1 e2) = addrsInPExpr e1 ++ addrsInPExpr e2
addrsInPPred (PEq  e1 e2) = addrsInPExpr e1 ++ addrsInPExpr e2
addrsInPPred (PGt  e1 e2) = addrsInPExpr e1 ++ addrsInPExpr e2
addrsInPPred (PGe  e1 e2) = addrsInPExpr e1 ++ addrsInPExpr e2
addrsInPPred (PNot p)     = addrsInPPred p
addrsInPPred (PAnd p q)   = addrsInPPred p ++ addrsInPPred q

-- ── SBV translation ───────────────────────────────────────────────────────────

pexprToSBV :: Map.Map Addr SInteger -> PExpr -> SInteger
pexprToSBV _  (Lit n)     = fromIntegral n
pexprToSBV hv (ValAt a)   = hv Map.! a
pexprToSBV hv (Add e1 e2) = pexprToSBV hv e1 + pexprToSBV hv e2
pexprToSBV hv (Mul k e)   = fromIntegral k * pexprToSBV hv e

ppredToSBV :: Map.Map Addr SInteger -> PPred -> SBool
ppredToSBV _  PTrue        = sTrue
ppredToSBV hv (PLt  e1 e2) = pexprToSBV hv e1 .< pexprToSBV hv e2
ppredToSBV hv (PLe  e1 e2) = pexprToSBV hv e1 .<= pexprToSBV hv e2
ppredToSBV hv (PEq  e1 e2) = pexprToSBV hv e1 .== pexprToSBV hv e2
ppredToSBV hv (PGt  e1 e2) = pexprToSBV hv e1 .> pexprToSBV hv e2
ppredToSBV hv (PGe  e1 e2) = pexprToSBV hv e1 .>= pexprToSBV hv e2
ppredToSBV hv (PNot p)     = sNot (ppredToSBV hv p)
ppredToSBV hv (PAnd p q)   = ppredToSBV hv p .&& ppredToSBV hv q

-- ── Core PPred solver ─────────────────────────────────────────────────────────
-- All ValAt references become unbounded integer variables.

checkPPred :: PPred -> IO SolverResult
checkPPred p = do
    let addrs   = nub (addrsInPPred p)
        varFor a = "h" ++ show a
    eResult <- try $ sat $ do
        vars <- mapM (sInteger . varFor) addrs
        let hv = Map.fromList (zip addrs vars)
        return (ppredToSBV hv p)
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

