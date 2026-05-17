{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.Capability where
import Prelude hiding ((<>))
import Pledge

requestToken :: String -> Pledge RE ()
requestToken user = Pledge
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "requestToken" (List [Str user]))
    , future = \_ -> finally (Atom "revokeToken" (List [Str user]))
    }

useToken :: String -> String -> Pledge RE ()
useToken user resource = Pledge
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "accessResource" (List [Str user, Str resource]))
    , future = const universe
    }

-- Precondition: a token must have just been requested for this user
revokeToken :: String -> Pledge RE ()
revokeToken user = Pledge
    { ret    = ()
    , pre    = Single (Atom "requestToken" (List [Str user]))
    , post   = Single (Atom "revokeToken" (List [Str user]))
    , future = const universe
    }

escalate :: String -> Pledge RE ()
escalate role = Pledge
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "escalate" (List [Str role]))
    , future = \_ -> finally (Atom "deescalate" (List [Str role]))
    }

deescalate :: String -> Pledge RE ()
deescalate role = Pledge
    { ret    = ()
    , pre    = Single (Atom "escalate" (List [Str role]))
    , post   = Single (Atom "deescalate" (List [Str role]))
    , future = const universe
    }

-- Good: token acquired and immediately revoked
properTokenUse :: Pledge RE ()
properTokenUse = do
    requestToken "alice"
    revokeToken "alice"

-- Good: privilege escalated and dropped
safeEscalation :: Pledge RE ()
safeEscalation = do
    escalate "admin"
    deescalate "admin"

-- Bad: token never revoked — future obligation remains
tokenLeak :: Pledge RE ()
tokenLeak = do
    requestToken "mallory"
    useToken "mallory" "/secrets"

-- Bad: privilege escalated but never dropped — future remains
privilegeLeak :: Pledge RE ()
privilegeLeak = do
    escalate "admin"
    useToken "system" "/root"

-- Bad: revokeToken without requestToken — precondition violated
revokeWithoutRequest :: Pledge RE ()
revokeWithoutRequest = revokeToken "eve"

printResult :: String -> Pledge RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre    prog))
    putStrLn $ "Post:   " ++ show (normalize (post   prog))
    putStrLn $ "Future: " ++ show (normalize (evalFuture prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "properTokenUse"       properTokenUse
    printResult "safeEscalation"       safeEscalation
    printResult "tokenLeak"            tokenLeak
    printResult "privilegeLeak"        privilegeLeak
    printResult "revokeWithoutRequest" revokeWithoutRequest
