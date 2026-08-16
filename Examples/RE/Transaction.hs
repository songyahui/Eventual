{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.Transaction where
import Prelude hiding ((<>))
import Pledge

-- Begin a transaction: future = eventually commit or rollback
beginTx :: Pledge IO (RE Term) ()
beginTx = Pledge $ return
    ((), universe,
     Single (Atom "beginTx" (List [])),
     Or (finally (Atom "commit" (List []))) (finally (Atom "rollback" (List []))))

dbWrite :: String -> Int -> Pledge IO (RE Term) ()
dbWrite key val = Pledge $ return
    ((), universe,
     Single (Atom "write" (List [Str key, Num val])),
     universe)

-- Precondition: a write must have just occurred (commit requires at least one write)
commit :: Pledge IO (RE Term) ()
commit = Pledge $ return
    ((), Or (Single (Atom "beginTx" (List [])))
            (Single (Atom "write"   (List []))),  -- wildcard args checked by RE matching
     Single (Atom "commit" (List [])),
     universe)

rollback :: Pledge IO (RE Term) ()
rollback = Pledge $ return
    ((), universe, Single (Atom "rollback" (List [])), universe)

-- Good: begin, write, commit
committedTx :: Pledge IO (RE Term) ()
committedTx = do
    beginTx
    dbWrite "balance" 100
    commit

-- Good: begin, write, rollback
rolledBackTx :: Pledge IO (RE Term) ()
rolledBackTx = do
    beginTx
    dbWrite "balance" 100
    rollback

-- Bad: begin and write but no commit or rollback — future obligation remains
openTx :: Pledge IO (RE Term) ()
openTx = do
    beginTx
    dbWrite "balance" 100

printResult :: String -> Pledge IO (RE Term) () -> IO ()
printResult name prog = do
    (_, preC, postC, futC) <- runPledge prog
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize preC)
    putStrLn $ "Post:   " ++ show (normalize postC)
    putStrLn $ "Future: " ++ show (normalize futC)
    putStrLn ""

main :: IO ()
main = do
    printResult "committedTx"  committedTx
    printResult "rolledBackTx" rolledBackTx
    printResult "openTx"       openTx
