{-# OPTIONS_GHC -i.. #-}
module Examples.Capability where
import Prelude hiding ((<>))
import Future

requestToken :: String -> Effectful RE ()
requestToken user = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "requestToken" [Str user])
    , future = finally (Atom "revokeToken" [Str user])
    }

useToken :: String -> String -> Effectful RE ()
useToken user resource = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "accessResource" [Str user, Str resource])
    , future = universe
    }

-- Precondition: a token must have just been requested for this user
revokeToken :: String -> Effectful RE ()
revokeToken user = Effectful
    { ret    = ()
    , pre    = Single (Atom "requestToken" [Str user])
    , post   = Single (Atom "revokeToken" [Str user])
    , future = universe
    }

escalate :: String -> Effectful RE ()
escalate role = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "escalate" [Str role])
    , future = finally (Atom "deescalate" [Str role])
    }

deescalate :: String -> Effectful RE ()
deescalate role = Effectful
    { ret    = ()
    , pre    = Single (Atom "escalate" [Str role])
    , post   = Single (Atom "deescalate" [Str role])
    , future = universe
    }

-- Good: token acquired and immediately revoked
properTokenUse :: Effectful RE ()
properTokenUse = do
    requestToken "alice"
    revokeToken "alice"

-- Good: privilege escalated and dropped
safeEscalation :: Effectful RE ()
safeEscalation = do
    escalate "admin"
    deescalate "admin"

-- Bad: token never revoked — future obligation remains
tokenLeak :: Effectful RE ()
tokenLeak = do
    requestToken "mallory"
    useToken "mallory" "/secrets"

-- Bad: privilege escalated but never dropped — future remains
privilegeLeak :: Effectful RE ()
privilegeLeak = do
    escalate "admin"
    useToken "system" "/root"

-- Bad: revokeToken without requestToken — precondition violated
revokeWithoutRequest :: Effectful RE ()
revokeWithoutRequest = revokeToken "eve"

printResult :: String -> Effectful RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre    prog))
    putStrLn $ "Post:   " ++ show (normalize (post   prog))
    putStrLn $ "Future: " ++ show (normalize (future prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "properTokenUse"       properTokenUse
    printResult "safeEscalation"       safeEscalation
    printResult "tokenLeak"            tokenLeak
    printResult "privilegeLeak"        privilegeLeak
    printResult "revokeWithoutRequest" revokeWithoutRequest
