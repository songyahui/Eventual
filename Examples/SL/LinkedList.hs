module Examples.SL.LinkedList where
import Prelude hiding ((<>))
import Pledge

-- ── Model ─────────────────────────────────────────────────────────────────────
-- A singly-linked list node at address `addr` occupies two consecutive cells:
--   Cell addr     val   — the payload value
--   Cell (addr+1) next  — the next pointer (-1 = NULL)

-- ── Primitives ────────────────────────────────────────────────────────────────

-- allocNode addr val next: allocates a fresh node.
-- Post: SepStar of the two cells (disjoint ownership).
allocNode :: Addr -> Val -> Val -> Pledge IO SL ()
allocNode addr val next = Pledge $ return
    ((), Top, SepStar (Cell addr val) (Cell (addr+1) next), Top)

-- freeNode addr val next: releases a node.
-- Pre:  own both cells of the node.
-- Post: Emp.
freeNode :: Addr -> Val -> Val -> Pledge IO SL ()
freeNode addr val next = Pledge $ return
    ((), SepStar (Cell addr val) (Cell (addr+1) next), Emp, Top)

-- readVal addr val: reads the payload; requires (and produces nothing from) val cell.
readVal :: Addr -> Val -> Pledge IO SL ()
readVal addr val = Pledge $ return ((), Cell addr val, Emp, Top)

-- readNext addr next: reads the next pointer.
readNext :: Addr -> Val -> Pledge IO SL ()
readNext addr next = Pledge $ return ((), Cell (addr+1) next, Emp, Top)

-- updateNext addr oldNext newNext: rewires the next pointer.
-- Pre:  own the next cell with oldNext.
-- Post: Cell (addr+1) newNext.
updateNext :: Addr -> Val -> Val -> Pledge IO SL ()
updateNext addr oldNext newNext = Pledge $ return
    ((), Cell (addr+1) oldNext, Cell (addr+1) newNext, Top)

-- ── Programs ──────────────────────────────────────────────────────────────────

-- Good: single-node list [10] at addr 0, terminated with -1.
singleNode :: Pledge IO SL ()
singleNode = allocNode 0 10 (-1)

-- Good: two-node list [10 -> 20] at addrs 0 and 2.
twoNodeList :: Pledge IO SL ()
twoNodeList = do
    allocNode 0 10 2      -- head: val=10, next→addr 2
    allocNode 2 20 (-1)   -- tail: val=20, next=NULL

-- Good: three-node list [5 -> 15 -> 25].
threeNodeList :: Pledge IO SL ()
threeNodeList = do
    allocNode 0 5  2
    allocNode 2 15 4
    allocNode 4 25 (-1)

-- Good: alloc a node then immediately free it.
allocFreeNode :: Pledge IO SL ()
allocFreeNode = do
    allocNode 0 42 (-1)
    freeNode  0 42 (-1)

-- Good: build a two-node list then rewire head's next to NULL (unlink tail).
unlinkTail :: Pledge IO SL ()
unlinkTail = do
    allocNode  0 10 2
    allocNode  2 20 (-1)
    updateNext 0 2  (-1)   -- head now points to NULL

-- Bad: read a node's value without owning it — pre = Cell 0 99, unmet.
readWithoutOwnership :: Pledge IO SL ()
readWithoutOwnership = readVal 0 99

-- Bad: free a node that was never allocated — pre = SepStar (Cell 0 1) (Cell 1 (-1)), unmet.
freeWithoutAlloc :: Pledge IO SL ()
freeWithoutAlloc = freeNode 0 1 (-1)

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
    printResult "singleNode"            singleNode
    printResult "twoNodeList"           twoNodeList
    printResult "threeNodeList"         threeNodeList
    printResult "allocFreeNode"         allocFreeNode
    printResult "unlinkTail"            unlinkTail
    printResult "readWithoutOwnership"  readWithoutOwnership
    printResult "freeWithoutAlloc"      freeWithoutAlloc
