{-# OPTIONS_GHC -i../.. #-}
module Examples.SL.HeapMemory where
import Prelude hiding ((<>))
import FutureCond

-- ── Primitives ────────────────────────────────────────────────────────────────
-- Each operation is modelled as an FutureCond SL computation.
-- post  = heap ownership this operation establishes
-- pre   = heap ownership required before this operation
-- future = heap obligation that must hold when the whole computation ends

-- alloc addr val: creates a fresh cell; requires nothing, produces ownership.
alloc :: Addr -> Val -> FutureCond SL ()
alloc addr val = FutureCond
    { ret    = ()
    , pre    = Top
    , post   = Cell addr val
    , future = \_ -> Top
    }

-- free addr val: releases ownership of a cell.
-- Pre:  must already own Cell addr val.
-- Post: Emp — ownership is relinquished.
free :: Addr -> Val -> FutureCond SL ()
free addr val = FutureCond
    { ret    = ()
    , pre    = Cell addr val
    , post   = Emp
    , future = \_ -> Top
    }

-- readCell addr val: borrows a cell for reading; produces no new ownership.
-- Pre:  must own Cell addr val.
-- Post: Emp — caller's ownership is separate and tracked via pre.
readCell :: Addr -> Val -> FutureCond SL ()
readCell addr val = FutureCond
    { ret    = ()
    , pre    = Cell addr val
    , post   = Emp
    , future = \_ -> Top
    }

-- writeCell addr old new: overwrites a cell.
-- Pre:  must own Cell addr old.
-- Post: Cell addr new — ownership updated to the new value.
writeCell :: Addr -> Val -> Val -> FutureCond SL ()
writeCell addr old new = FutureCond
    { ret    = ()
    , pre    = Cell addr old
    , post   = Cell addr new
    , future = \_ -> Top
    }

-- ── Programs ──────────────────────────────────────────────────────────────────

-- Good: alloc then immediately free.
-- Post normalises to Emp; pre residual is Wand (Cell 0 42) (Cell 0 42).
allocFree :: FutureCond SL ()
allocFree = do
    alloc 0 42
    free  0 42

-- Good: alloc two disjoint cells.
-- Post = SepStar (Cell 0 1) (Cell 1 2): two disjoint ownerships.
allocTwo :: FutureCond SL ()
allocTwo = do
    alloc 0 1
    alloc 1 2

-- Good: write a value then read it back.
-- Pre:  Cell 0 0 (old value must exist).
-- Post: Emp (read produces nothing; write consumed old, produced new).
writeRead :: FutureCond SL ()
writeRead = do
    writeCell 0 0 99
    readCell  0 99

-- Good: alloc, write, free — full ownership cycle.
-- Post normalises to Emp.
fullCycle :: FutureCond SL ()
fullCycle = do
    alloc     0 0
    writeCell 0 0 7
    free      0 7

-- Bad: read without ownership — pre = Cell 0 42, unsatisfied.
readWithoutAlloc :: FutureCond SL ()
readWithoutAlloc = readCell 0 42

-- Bad: alloc two cells, free only one — Cell 1 2 ownership remains in post.
leakOne :: FutureCond SL ()
leakOne = do
    alloc 0 1
    alloc 1 2
    free  0 1   -- Cell 1 2 still owned

-- ── Display ───────────────────────────────────────────────────────────────────

printResult :: String -> FutureCond SL () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalizeSL (pre    prog))
    putStrLn $ "Post:   " ++ show (normalizeSL (post   prog))
    putStrLn $ "Future: " ++ show (normalizeSL (evalFuture prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "allocFree"          allocFree
    printResult "allocTwo"           allocTwo
    printResult "writeRead"          writeRead
    printResult "fullCycle"          fullCycle
    printResult "readWithoutAlloc"   readWithoutAlloc
    printResult "leakOne"            leakOne
