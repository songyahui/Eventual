{-# OPTIONS_GHC -i../.. #-}
module Examples.SL.LinkedList where
import Prelude hiding ((<>))
import Future

-- ── Model ─────────────────────────────────────────────────────────────────────
-- A singly-linked list node at address `addr` occupies two consecutive cells:
--   Cell addr     val   — the payload value
--   Cell (addr+1) next  — the next pointer (-1 = NULL)
--
-- A list segment from addr to NULL is represented by SepStar of all its nodes.

-- ── Primitives ────────────────────────────────────────────────────────────────

-- allocNode addr val next: allocates a fresh node.
-- Post: SepStar of the two cells (disjoint ownership).
allocNode :: Addr -> Val -> Val -> Effectful SL ()
allocNode addr val next = Effectful
    { ret    = ()
    , pre    = Top
    , post   = SepStar (Cell addr val) (Cell (addr+1) next)
    , future = Top
    }

-- freeNode addr val next: releases a node.
-- Pre:  own both cells of the node.
-- Post: Emp.
freeNode :: Addr -> Val -> Val -> Effectful SL ()
freeNode addr val next = Effectful
    { ret    = ()
    , pre    = SepStar (Cell addr val) (Cell (addr+1) next)
    , post   = Emp
    , future = Top
    }

-- readVal addr val: reads the payload; requires (and produces nothing from) val cell.
readVal :: Addr -> Val -> Effectful SL ()
readVal addr val = Effectful
    { ret    = ()
    , pre    = Cell addr val
    , post   = Emp
    , future = Top
    }

-- readNext addr next: reads the next pointer.
readNext :: Addr -> Val -> Effectful SL ()
readNext addr next = Effectful
    { ret    = ()
    , pre    = Cell (addr+1) next
    , post   = Emp
    , future = Top
    }

-- updateNext addr oldNext newNext: rewires the next pointer.
-- Pre:  own the next cell with oldNext.
-- Post: Cell (addr+1) newNext.
updateNext :: Addr -> Val -> Val -> Effectful SL ()
updateNext addr oldNext newNext = Effectful
    { ret    = ()
    , pre    = Cell (addr+1) oldNext
    , post   = Cell (addr+1) newNext
    , future = Top
    }

-- ── Programs ──────────────────────────────────────────────────────────────────

-- Good: single-node list [10] at addr 0, terminated with -1.
-- Post: SepStar (Cell 0 10) (Cell 1 (-1))
singleNode :: Effectful SL ()
singleNode = allocNode 0 10 (-1)

-- Good: two-node list [10 -> 20] at addrs 0 and 2.
-- Node 0: val=10, next=2
-- Node 2: val=20, next=-1
-- Post: SepStar of four cells (two nodes, each two cells).
twoNodeList :: Effectful SL ()
twoNodeList = do
    allocNode 0 10 2      -- head: val=10, next→addr 2
    allocNode 2 20 (-1)   -- tail: val=20, next=NULL

-- Good: three-node list [5 -> 15 -> 25].
threeNodeList :: Effectful SL ()
threeNodeList = do
    allocNode 0 5  2
    allocNode 2 15 4
    allocNode 4 25 (-1)

-- Good: alloc a node then immediately free it.
-- Post normalises toward Emp; pre carries the Wand residual.
allocFreeNode :: Effectful SL ()
allocFreeNode = do
    allocNode 0 42 (-1)
    freeNode  0 42 (-1)

-- Good: build a two-node list then rewire head's next to NULL (unlink tail).
-- Before unlink: 0→2→NULL.  After: 0→NULL (tail still allocated but unreachable).
unlinkTail :: Effectful SL ()
unlinkTail = do
    allocNode  0 10 2
    allocNode  2 20 (-1)
    updateNext 0 2  (-1)   -- head now points to NULL

-- Bad: read a node's value without owning it — pre = Cell 0 99, unmet.
readWithoutOwnership :: Effectful SL ()
readWithoutOwnership = readVal 0 99

-- Bad: free a node that was never allocated — pre = SepStar (Cell 0 1) (Cell 1 (-1)), unmet.
freeWithoutAlloc :: Effectful SL ()
freeWithoutAlloc = freeNode 0 1 (-1)

-- ── Display ───────────────────────────────────────────────────────────────────

printResult :: String -> Effectful SL () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalizeSL (pre    prog))
    putStrLn $ "Post:   " ++ show (normalizeSL (post   prog))
    putStrLn $ "Future: " ++ show (normalizeSL (future prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "singleNode"            singleNode
    printResult "twoNodeList"           twoNodeList
    printResult "threeNodeList"         threeNodeList
    printResult "allocFreeNode"         allocFreeNode
    printResult "unlinkTail"            unlinkTail
    printResult "readWithoutOwnership"  readWithoutOwnership
    printResult "freeWithoutAlloc"      freeWithoutAlloc
