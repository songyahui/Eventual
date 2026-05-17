{-# OPTIONS_GHC -i../.. #-}
import Prelude hiding ((<>))
import Data.IORef
import Pledge
import Solver

-- ── Test infrastructure ───────────────────────────────────────────────────────

check :: IORef Int -> IORef Int -> String -> IO Bool -> IO ()
check total failures name action = do
    modifyIORef' total (+1)
    ok <- action
    if ok
        then putStrLn $ "  PASS  " ++ name
        else do
            modifyIORef' failures (+1)
            putStrLn $ "  FAIL  " ++ name

expectSat :: IO SolverResult -> IO Bool
expectSat action = do
    r <- action
    case r of
        Satisfied _ -> return True
        _           -> do putStrLn $ "        (got: " ++ show r ++ ")"
                          return False

expectUnsat :: IO SolverResult -> IO Bool
expectUnsat action = do
    r <- action
    case r of
        Unsatisfiable -> return True
        _             -> do putStrLn $ "        (got: " ++ show r ++ ")"
                            return False

-- ── §1  Pure Presburger predicate tests ──────────────────────────────────────
-- These test checkPPred in isolation, with no SL heap-ownership structure.

test_purePreds :: IORef Int -> IORef Int -> IO ()
test_purePreds total failures = do
    putStrLn "\n── §1  Pure Presburger predicates ───────────────────────────────────"
    let chk n a = check total failures n a

    -- Trivially satisfiable
    chk "PTrue  (SAT)" $
        expectSat (checkPPred PTrue)

    -- Simple inequality: h[0] > 5
    chk "h[0] > 5  (SAT)" $
        expectSat (checkPPred (PGt (ValAt 0) (Lit 5)))

    -- Contradiction: h[0] > 5 ∧ h[0] < 3
    chk "h[0] > 5 ∧ h[0] < 3  (UNSAT)" $
        expectUnsat (checkPPred (PAnd (PGt (ValAt 0) (Lit 5))
                                      (PLt (ValAt 0) (Lit 3))))

    -- Two-variable sum: h[0] + h[1] = 10  (many solutions)
    chk "h[0] + h[1] = 10  (SAT)" $
        expectSat (checkPPred (PEq (Add (ValAt 0) (ValAt 1)) (Lit 10)))

    -- Scalar multiplication: 2*h[0] = 8  (h[0] = 4)
    chk "2*h[0] = 8  (SAT)" $
        expectSat (checkPPred (PEq (Mul 2 (ValAt 0)) (Lit 8)))

    -- Unsatisfiable system: h[0]+h[1]=10 ∧ h[0]=h[1] ∧ h[0]>6
    -- (sum of equals must each be 5, contradicts >6)
    chk "h[0]+h[1]=10 ∧ h[0]=h[1] ∧ h[0]>6  (UNSAT)" $
        expectUnsat (checkPPred
            (PAnd (PEq (Add (ValAt 0) (ValAt 1)) (Lit 10))
            (PAnd (PEq (ValAt 0) (ValAt 1))
                  (PGt (ValAt 0) (Lit 6)))))

    -- Negation: ¬(h[0] = h[0]) is always false
    chk "¬(h[0] = h[0])  (UNSAT)" $
        expectUnsat (checkPPred (PNot (PEq (ValAt 0) (ValAt 0))))

    -- Negation: ¬(h[0] > 100) is satisfiable (e.g. h[0]=0)
    chk "¬(h[0] > 100)  (SAT)" $
        expectSat (checkPPred (PNot (PGt (ValAt 0) (Lit 100))))

    -- Three-variable: h[0] + h[1] + h[2] = 0 ∧ h[0]≥0 ∧ h[1]≥0 ∧ h[2]≥0
    -- (forces all three to zero)
    chk "h[i]≥0 for i=0,1,2 and sum=0  (SAT)" $
        expectSat (checkPPred
            (PAnd (PEq (Add (ValAt 0) (Add (ValAt 1) (ValAt 2))) (Lit 0))
            (PAnd (PGe (ValAt 0) (Lit 0))
            (PAnd (PGe (ValAt 1) (Lit 0))
                  (PGe (ValAt 2) (Lit 0))))))

    -- Diophantine: 3*h[0] - 2*h[1] = 1  (has integer solutions)
    chk "3*h[0] - 2*h[1] = 1  (SAT)" $
        expectSat (checkPPred
            (PEq (Add (Mul 3 (ValAt 0)) (Mul (-2) (ValAt 1))) (Lit 1)))

-- ── §2  Bank account SL precondition tests ────────────────────────────────────
-- We directly construct the SL predicate that would appear in `pre` for each
-- banking operation and ask whether it is satisfiable.

test_bankAccount :: IORef Int -> IORef Int -> IO ()
test_bankAccount total failures = do
    putStrLn "\n── §2  Bank account: SL pre-condition discharge ──────────────────────"
    let chk n a = check total failures n a

    -- Valid withdrawal: Cell 0 100 ∧ Pure(h[0] ≥ 40)
    -- Cell pins h[0]=100; the guard 100 ≥ 40 holds.
    let validWithdrawPre = Conj (Pure (PGe (ValAt 0) (Lit 40))) (Cell 0 100)
    chk "withdraw 40 from balance 100: pre SAT" $
        expectSat (checkSL validWithdrawPre)

    -- Overdraft: Cell 0 100 ∧ Pure(h[0] ≥ 150)
    -- Cell pins h[0]=100; 100 ≥ 150 is false → UNSAT.
    let overdraftPre = Conj (Pure (PGe (ValAt 0) (Lit 150))) (Cell 0 100)
    chk "overdraft: withdraw 150 from balance 100: pre UNSAT" $
        expectUnsat (checkSL overdraftPre)

    -- Close empty account: Cell 0 0 ∧ Pure(h[0] = 0) — consistent.
    let closeEmptyPre = Conj (Pure (PEq (ValAt 0) (Lit 0))) (Cell 0 0)
    chk "close account with balance 0: pre SAT" $
        expectSat (checkSL closeEmptyPre)

    -- Close non-empty account: Cell 0 30 ∧ Pure(h[0] = 0) — contradiction.
    let closeNonEmptyPre = Conj (Pure (PEq (ValAt 0) (Lit 0))) (Cell 0 30)
    chk "close account with balance 30: pre UNSAT" $
        expectUnsat (checkSL closeNonEmptyPre)

    -- Transfer with sufficient funds:
    -- SepStar (Cell 0 200) (Cell 1 0) ∧ Pure(h[0] ≥ 75)
    let transferOkPre = Conj (Pure (PGe (ValAt 0) (Lit 75)))
                             (SepStar (Cell 0 200) (Cell 1 0))
    chk "transfer 75 from account-0 (bal=200): pre SAT" $
        expectSat (checkSL transferOkPre)

    -- Transfer exceeding balance:
    -- SepStar (Cell 0 200) (Cell 1 0) ∧ Pure(h[0] ≥ 250)
    let transferBadPre = Conj (Pure (PGe (ValAt 0) (Lit 250)))
                              (SepStar (Cell 0 200) (Cell 1 0))
    chk "transfer 250 from account-0 (bal=200): pre UNSAT" $
        expectUnsat (checkSL transferBadPre)

-- ── §3  Multi-account Presburger constraints ──────────────────────────────────

test_multiAccount :: IORef Int -> IORef Int -> IO ()
test_multiAccount total failures = do
    putStrLn "\n── §3  Multi-account constraints ────────────────────────────────────"
    let chk n a = check total failures n a

    -- Combined balance check: h[0]+h[1] ≥ 100, with h[0]=60, h[1]=50 (sum=110).
    let combinedOkPre = Conj (Pure (PGe (Add (ValAt 0) (ValAt 1)) (Lit 100)))
                             (SepStar (Cell 0 60) (Cell 1 50))
    chk "combined balance 60+50=110 ≥ 100: SAT" $
        expectSat (checkSL combinedOkPre)

    -- Combined balance check fails: h[0]=30, h[1]=50 (sum=80 < 100).
    let combinedBadPre = Conj (Pure (PGe (Add (ValAt 0) (ValAt 1)) (Lit 100)))
                              (SepStar (Cell 0 30) (Cell 1 50))
    chk "combined balance 30+50=80 ≥ 100: UNSAT" $
        expectUnsat (checkSL combinedBadPre)

    -- Non-negative residual: h[0] - 40 ≥ 0 ∧ h[0]=100 (i.e. withdrawal leaves ≥ 0).
    -- Encoded as h[0] + (-1)*40 ≥ 0 using Mul.
    let nonNegPre = Conj (Pure (PGe (Add (ValAt 0) (Mul (-1) (Lit 40))) (Lit 0)))
                         (Cell 0 100)
    chk "residual balance after withdraw-40 (bal=100) ≥ 0: SAT" $
        expectSat (checkSL nonNegPre)

    -- Equality mismatch across accounts: Pure(h[0]=h[1]) ∧ Cell 0 5 ∧ Cell 1 10.
    let mismatchPre = Conj (Pure (PEq (ValAt 0) (ValAt 1)))
                           (SepStar (Cell 0 5) (Cell 1 10))
    chk "require h[0]=h[1] but Cell 0 5, Cell 1 10: UNSAT" $
        expectUnsat (checkSL mismatchPre)

    -- Three accounts: ensure total ≤ 500, individual balances h[i]=150.
    let threeCells =       SepStar (Cell 0 150) (SepStar (Cell 1 150) (Cell 2 150))
        threePre   = Conj (Pure (PLe (Add (ValAt 0) (Add (ValAt 1) (ValAt 2))) (Lit 500)))
                          threeCells
    chk "three accounts: 150+150+150=450 ≤ 500: SAT" $
        expectSat (checkSL threePre)

    -- Same setup but require total ≤ 400 (450 > 400 → UNSAT).
    let threePreBad = Conj (Pure (PLe (Add (ValAt 0) (Add (ValAt 1) (ValAt 2))) (Lit 400)))
                           threeCells
    chk "three accounts: 150+150+150=450 ≤ 400: UNSAT" $
        expectUnsat (checkSL threePreBad)

-- ── §4  Pledge-level helper tests ─────────────────────────────────────────────
-- Build small Pledge SL programs directly and discharge via checkPledgePre.

-- Valid sequence: open then deposit
depositPledge :: Pledge SL ()
depositPledge = do
    Pledge { ret = (), pre = Top, post = Cell 0 0, future = \_ -> Top }
    Pledge { ret = (), pre = Cell 0 0, post = Cell 0 100, future = \_ -> Top }

-- Directly contradictory pre (no monadic composition involved).
-- Monadic bind would wrap the second step's pre in a Wand, hiding the
-- contradiction from Presburger discharge.  We test the primitive instead.
directOverdraftPledge :: Pledge SL ()
directOverdraftPledge = Pledge
    { ret    = ()
    , pre    = Conj (Pure (PGe (ValAt 0) (Lit 150))) (Cell 0 100)
    , post   = Cell 0 (-50)
    , future = \_ -> Top
    }

-- Composed overdraft: monadic bind wraps the second step's pre in a Wand.
-- slToPPred conservatively maps Wand to PTrue, so the result is SAT.
-- This demonstrates the Wand limitation documented in Solver.hs.
composedOverdraftPledge :: Pledge SL ()
composedOverdraftPledge = do
    Pledge { ret = (), pre = Top, post = Cell 0 0, future = \_ -> Top }
    Pledge { ret    = ()
           , pre    = Conj (Pure (PGe (ValAt 0) (Lit 150))) (Cell 0 100)
           , post   = Cell 0 (-50)
           , future = \_ -> Top
           }

test_pledgeHelpers :: IORef Int -> IORef Int -> IO ()
test_pledgeHelpers total failures = do
    putStrLn "\n── §4  Pledge-level checkPledgePre / checkPledgeFuture ───────────────"
    let chk n a = check total failures n a

    -- depositPledge has a satisfiable pre (Top after normalization)
    chk "depositPledge: pre SAT" $
        expectSat (checkPledgePre depositPledge)

    -- depositPledge has Top future → always SAT
    chk "depositPledge: future SAT" $
        expectSat (checkPledgeFuture depositPledge)

    -- Direct overdraft pledge (not composed) — contradiction visible in pre → UNSAT
    chk "direct overdraft pledge: pre UNSAT" $
        expectUnsat (checkPledgePre directOverdraftPledge)

    -- After monadic bind the second step's pre becomes Wand(Cell 0 0, Conj(...)).
    -- Wand is beyond Presburger and is conservatively mapped to PTrue → SAT.
    chk "composed overdraft pledge: pre SAT (Wand limits discharge)" $
        expectSat (checkPledgePre composedOverdraftPledge)

-- ── §5  Witness display ───────────────────────────────────────────────────────
-- For SAT cases, print the concrete witness heap that Z3 produces.
-- This demonstrates the counterexample / diagnostic capability.

test_witnessDisplay :: IO ()
test_witnessDisplay = do
    putStrLn "\n── §5  Witness display (concrete counterexamples from Z3) ───────────"

    let cases :: [(String, IO SolverResult)]
        cases =
            [ ( "h[0] > 10"
              , checkPPred (PGt (ValAt 0) (Lit 10)) )

            , ( "h[0] > 5 ∧ h[0] < 3  (expected UNSAT)"
              , checkPPred (PAnd (PGt (ValAt 0) (Lit 5)) (PLt (ValAt 0) (Lit 3))) )

            , ( "overdraft: h[0] ≥ 150 ∧ h[0] = 100  (expected UNSAT)"
              , checkSL (Conj (Pure (PGe (ValAt 0) (Lit 150))) (Cell 0 100)) )

            , ( "valid withdraw: h[0] ≥ 40 ∧ h[0] = 100"
              , checkSL (Conj (Pure (PGe (ValAt 0) (Lit 40))) (Cell 0 100)) )

            , ( "h[0] + h[1] = 50 ∧ h[0] = 2*h[1]  (UNSAT over ℤ: 3∤50)"
              , checkPPred (PAnd (PEq (Add (ValAt 0) (ValAt 1)) (Lit 50))
                                 (PEq (ValAt 0) (Mul 2 (ValAt 1)))) )

            , ( "3-account total ≤ 400, each bal=150  (expected UNSAT)"
              , checkSL (Conj
                    (Pure (PLe (Add (ValAt 0) (Add (ValAt 1) (ValAt 2))) (Lit 400)))
                    (SepStar (Cell 0 150) (SepStar (Cell 1 150) (Cell 2 150)))) )
            ]

    mapM_ (\(label, action) -> do
        r <- action
        putStrLn $ "  " ++ label
        putStrLn $ "  → " ++ show r
        putStrLn "") cases

-- ── Main ──────────────────────────────────────────────────────────────────────

main :: IO ()
main = do
    putStrLn "=== Presburger / SL Solver Tests ==="
    total    <- newIORef (0 :: Int)
    failures <- newIORef (0 :: Int)

    test_purePreds     total failures
    test_bankAccount   total failures
    test_multiAccount  total failures
    test_pledgeHelpers total failures

    t <- readIORef total
    f <- readIORef failures
    putStrLn $ "\n" ++ show (t - f) ++ "/" ++ show t ++ " tests passed."

    test_witnessDisplay
