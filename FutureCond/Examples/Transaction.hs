{-# OPTIONS_GHC -i.. #-}
module Examples.Transaction where
import Prelude hiding ((<>))
import Future

-- Begin a transaction: future = eventually commit or rollback
beginTx :: Effectful RE ()
beginTx = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "beginTx" [])
    , future = Or (finally (Atom "commit" [])) (finally (Atom "rollback" []))
    }

dbWrite :: String -> Int -> Effectful RE ()
dbWrite key val = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "write" [Str key, Num val])
    , future = universe
    }

-- Precondition: a write must have just occurred (commit requires at least one write)
commit :: Effectful RE ()
commit = Effectful
    { ret    = ()
    , pre    = Or (Single (Atom "beginTx" []))
                  (Single (Atom "write"   []))  -- wildcard args checked by RE matching
    , post   = Single (Atom "commit" [])
    , future = universe
    }

rollback :: Effectful RE ()
rollback = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "rollback" [])
    , future = universe
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

-- Bad: begin and write but no commit or rollback — future obligation remains
openTx :: Effectful RE ()
openTx = do
    beginTx
    dbWrite "balance" 100

printResult :: String -> Effectful RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre    prog))
    putStrLn $ "Post:   " ++ show (normalize (post   prog))
    putStrLn $ "Future: " ++ show (normalize (future prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "committedTx"  committedTx
    printResult "rolledBackTx" rolledBackTx
    printResult "openTx"       openTx
