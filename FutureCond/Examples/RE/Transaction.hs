{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.Transaction where
import Prelude hiding ((<>))
import FutureCond

-- Begin a transaction: future = eventually commit or rollback
beginTx :: FutureCond RE ()
beginTx = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "beginTx" (List []))
    , future = \_ -> Or (finally (Atom "commit" (List []))) (finally (Atom "rollback" (List [])))
    }

dbWrite :: String -> Int -> FutureCond RE ()
dbWrite key val = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "write" (List [Str key, Num val]))
    , future = \_ -> universe
    }

-- Precondition: a write must have just occurred (commit requires at least one write)
commit :: FutureCond RE ()
commit = FutureCond
    { ret    = ()
    , pre    = Or (Single (Atom "beginTx" (List [])))
                  (Single (Atom "write"   (List [])))  -- wildcard args checked by RE matching
    , post   = Single (Atom "commit" (List []))
    , future = \_ -> universe
    }

rollback :: FutureCond RE ()
rollback = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "rollback" (List []))
    , future = \_ -> universe
    }

-- Good: begin, write, commit
committedTx :: FutureCond RE ()
committedTx = do
    beginTx
    dbWrite "balance" 100
    commit

-- Good: begin, write, rollback
rolledBackTx :: FutureCond RE ()
rolledBackTx = do
    beginTx
    dbWrite "balance" 100
    rollback

-- Bad: begin and write but no commit or rollback — future obligation remains
openTx :: FutureCond RE ()
openTx = do
    beginTx
    dbWrite "balance" 100

printResult :: String -> FutureCond RE () -> IO ()
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
