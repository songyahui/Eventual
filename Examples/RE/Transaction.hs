{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.Transaction where
import Prelude hiding ((<>))
import Pledge

-- Begin a transaction: future = eventually commit or rollback
beginTx :: Pledge RE ()
beginTx = Pledge
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "beginTx" (List []))
    , future = \_ -> Or (finally (Atom "commit" (List []))) (finally (Atom "rollback" (List [])))
    }

dbWrite :: String -> Int -> Pledge RE ()
dbWrite key val = Pledge
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "write" (List [Str key, Num val]))
    , future = const universe
    }

-- Precondition: a write must have just occurred (commit requires at least one write)
commit :: Pledge RE ()
commit = Pledge
    { ret    = ()
    , pre    = Or (Single (Atom "beginTx" (List [])))
                  (Single (Atom "write"   (List [])))  -- wildcard args checked by RE matching
    , post   = Single (Atom "commit" (List []))
    , future = const universe
    }

rollback :: Pledge RE ()
rollback = Pledge
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "rollback" (List []))
    , future = const universe
    }

-- Good: begin, write, commit
committedTx :: Pledge RE ()
committedTx = do
    beginTx
    dbWrite "balance" 100
    commit

-- Good: begin, write, rollback
rolledBackTx :: Pledge RE ()
rolledBackTx = do
    beginTx
    dbWrite "balance" 100
    rollback

-- Bad: begin and write but no commit or rollback — future obligation remains
openTx :: Pledge RE ()
openTx = do
    beginTx
    dbWrite "balance" 100

printResult :: String -> Pledge RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre    prog))
    putStrLn $ "Post:   " ++ show (normalize (post   prog))
    putStrLn $ "Future: " ++ show (normalize (evalFuture prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "committedTx"  committedTx
    printResult "rolledBackTx" rolledBackTx
    printResult "openTx"       openTx
