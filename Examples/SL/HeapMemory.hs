module Examples.SL.HeapMemory where
import Prelude hiding ((<>))
import Pledge

-- ── Primitives ────────────────────────────────────────────────────────────────
-- Each operation is modelled as a Pledge IO SL computation.
-- post  = heap ownership this operation establishes
-- pre   = heap ownership required before this operation
-- future = heap obligation that must hold when the whole computation ends

-- alloc addr val: creates a fresh cell; requires nothing, produces ownership.
alloc :: Addr -> Val -> Pledge IO SL ()
alloc addr val = Pledge $ return ((), Top, Cell addr val, Top)

-- free addr val: releases ownership of a cell.
-- Pre:  must already own Cell addr val.
-- Post: Emp — ownership is relinquished.
free :: Addr -> Val -> Pledge IO SL ()
free addr val = Pledge $ return ((), Cell addr val, Emp, Top)

-- readCell addr val: borrows a cell for reading; produces no new ownership.
-- Pre:  must own Cell addr val.
-- Post: Emp — caller's ownership is separate and tracked via pre.
readCell :: Addr -> Val -> Pledge IO SL ()
readCell addr val = Pledge $ return ((), Cell addr val, Emp, Top)

-- writeCell addr old new: overwrites a cell.
-- Pre:  must own Cell addr old.
-- Post: Cell addr new — ownership updated to the new value.
writeCell :: Addr -> Val -> Val -> Pledge IO SL ()
writeCell addr old new = Pledge $ return ((), Cell addr old, Cell addr new, Top)

-- ── Programs ──────────────────────────────────────────────────────────────────

-- Good: alloc then immediately free.
allocFree :: Pledge IO SL ()
allocFree = do
    alloc 0 42
    free  0 42

-- Good: alloc two disjoint cells.
-- Post = SepStar (Cell 0 1) (Cell 1 2): two disjoint ownerships.
allocTwo :: Pledge IO SL ()
allocTwo = do
    alloc 0 1
    alloc 1 2

-- Good: write a value then read it back.
-- Pre:  Cell 0 0 (old value must exist).
writeRead :: Pledge IO SL ()
writeRead = do
    writeCell 0 0 99
    readCell  0 99

-- Good: alloc, write, free — full ownership cycle.
fullCycle :: Pledge IO SL ()
fullCycle = do
    alloc     0 0
    writeCell 0 0 7
    free      0 7

-- Bad: read without ownership — pre = Cell 0 42, unsatisfied.
readWithoutAlloc :: Pledge IO SL ()
readWithoutAlloc = readCell 0 42

-- Bad: alloc two cells, free only one — Cell 1 2 ownership remains in post.
leakOne :: Pledge IO SL ()
leakOne = do
    alloc 0 1
    alloc 1 2
    free  0 1   -- Cell 1 2 still owned

-- ── Display ───────────────────────────────────────────────────────────────────

printResult :: String -> Pledge IO SL () -> IO ()
printResult name prog = do
    (_, preC, postC, futC) <- runPledge prog
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalizeSL preC)
    putStrLn $ "Post:   " ++ show (normalizeSL postC)
    putStrLn $ "Future: " ++ show (normalizeSL futC)
    putStrLn ""

main :: IO ()
main = do
    printResult "allocFree"          allocFree
    printResult "allocTwo"           allocTwo
    printResult "writeRead"          writeRead
    printResult "fullCycle"          fullCycle
    printResult "readWithoutAlloc"   readWithoutAlloc
    printResult "leakOne"            leakOne
