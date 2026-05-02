{-# OPTIONS_GHC -i.. #-}
module Examples.Transaction where
import Prelude hiding ((<>))
import Future

-- Begin a transaction: future = eventually commit or rollback
beginTx :: Effectful RE ()
beginTx = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("beginTx", [])
    , future = Or (finally ("commit", [])) (finally ("rollback", []))
    }

-- Write within a transaction: no new obligation
dbWrite :: String -> Int -> Effectful RE ()
dbWrite key val = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("write", [Str key, Num val])
    , future = anything
    }

-- Commit: discharges the beginTx future obligation
commit :: Effectful RE ()
commit = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("commit", [])
    , future = anything
    }

-- Rollback: also discharges the beginTx future obligation
rollback :: Effectful RE ()
rollback = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("rollback", [])
    , future = anything
    }

-- Good: begin, write, commit
committedTx :: Effectful RE ()
committedTx = do
    beginTx
    dbWrite "balance" 100
    commit

-- Good: begin, write, rollback
rolledBackTx :: Effectful RE ()
rolledBackTx = do
    beginTx
    dbWrite "balance" 100
    rollback

-- Bad: begin and write but no commit or rollback
openTx :: Effectful RE ()
openTx = do
    beginTx
    dbWrite "balance" 100

printResult :: String -> Effectful RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Post:   " ++ show (post prog)
    putStrLn $ "Future: " ++ show (normalize (future prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "committedTx"  committedTx
    printResult "rolledBackTx" rolledBackTx
    printResult "openTx"       openTx
