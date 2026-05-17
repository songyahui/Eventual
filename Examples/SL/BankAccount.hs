{-# OPTIONS_GHC -i../.. #-}
module Examples.SL.BankAccount where
import Prelude hiding ((<>))
import Pledge

-- ── Model ─────────────────────────────────────────────────────────────────────
-- The heap stores one cell per account: Cell addr balance.
-- Pure Presburger constraints guard withdrawals (sufficient balance)
-- and enforce invariants (balance never goes negative).

-- ── Primitives ────────────────────────────────────────────────────────────────

-- openAccount addr: creates an account with zero balance.
openAccount :: Addr -> Pledge SL ()
openAccount addr = Pledge
    { ret    = ()
    , pre    = Top
    , post   = Cell addr 0
    , future = const Top
    }

-- deposit addr old amount: unconditionally adds amount to the balance.
-- Pre:  own Cell addr old (any existing balance).
-- Post: Cell addr (old + amount).
deposit :: Addr -> Val -> Val -> Pledge SL ()
deposit addr old amount = Pledge
    { ret    = ()
    , pre    = Cell addr old
    , post   = Cell addr (old + amount)
    , future = const Top
    }

-- withdraw addr old amount: subtracts amount; requires balance >= amount.
-- Pre:  own Cell addr old AND h[addr] >= amount (Presburger guard).
-- Post: Cell addr (old - amount).
withdraw :: Addr -> Val -> Val -> Pledge SL ()
withdraw addr old amount = Pledge
    { ret    = ()
    , pre    = Conj (Pure (PGe (ValAt addr) (Lit amount)))
                    (Cell addr old)
    , post   = Cell addr (old - amount)
    , future = const Top
    }

-- transfer srcAddr srcBal dstAddr dstBal amount:
-- moves amount from src to dst; requires src has sufficient balance.
-- Pre:  own both cells; src balance >= amount.
-- Post: SepStar of updated cells (disjoint ownership).
transfer :: Addr -> Val -> Addr -> Val -> Val -> Pledge SL ()
transfer src srcBal dst dstBal amount = Pledge
    { ret    = ()
    , pre    = Conj (Pure (PGe (ValAt src) (Lit amount)))
                    (SepStar (Cell src srcBal) (Cell dst dstBal))
    , post   = SepStar (Cell src (srcBal - amount))
                       (Cell dst (dstBal + amount))
    , future = const Top
    }

-- closeAccount addr bal: closes account; requires balance is zero.
-- Pre:  own Cell addr bal AND balance = 0.
-- Post: Emp — ownership released.
closeAccount :: Addr -> Val -> Pledge SL ()
closeAccount addr bal = Pledge
    { ret    = ()
    , pre    = Conj (Pure (PEq (ValAt addr) (Lit 0)))
                    (Cell addr bal)
    , post   = Emp
    , future = const Top
    }

-- ── Programs ──────────────────────────────────────────────────────────────────

-- Good: open, deposit 100, withdraw 40, close with balance 60 still owned.
-- (Account not closed — balance remains in post.)
depositAndWithdraw :: Pledge SL ()
depositAndWithdraw = do
    openAccount 0
    deposit  0 0   100
    withdraw 0 100 40

-- Good: full lifecycle — open, deposit, withdraw back to zero, close.
fullLifecycle :: Pledge SL ()
fullLifecycle = do
    openAccount 0
    deposit      0 0   50
    withdraw     0 50  50
    closeAccount 0 0

-- Good: transfer between two accounts.
transferBetween :: Pledge SL ()
transferBetween = do
    openAccount 0
    openAccount 1
    deposit  0 0 200
    transfer 0 200 1 0 75

-- Bad: withdraw without sufficient balance — pure guard (h[0] >= 150) in pre
--      cannot be met when balance is only 100.
overdraft :: Pledge SL ()
overdraft = do
    openAccount 0
    deposit  0 0   100
    withdraw 0 100 150   -- pre includes Pure (h[0] >= 150), but h[0] = 100

-- Bad: close account with non-zero balance — Pure (h[0] = 0) in pre not met.
closeNonEmpty :: Pledge SL ()
closeNonEmpty = do
    openAccount 0
    deposit      0 0 30
    closeAccount 0 30   -- pre requires balance = 0

-- ── Display ───────────────────────────────────────────────────────────────────

printResult :: String -> Pledge SL () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalizeSL (pre    prog))
    putStrLn $ "Post:   " ++ show (normalizeSL (post   prog))
    putStrLn $ "Future: " ++ show (normalizeSL (evalFuture prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "depositAndWithdraw"  depositAndWithdraw
    printResult "fullLifecycle"       fullLifecycle
    printResult "transferBetween"     transferBetween
    printResult "overdraft (bad)"     overdraft
    printResult "closeNonEmpty (bad)" closeNonEmpty
