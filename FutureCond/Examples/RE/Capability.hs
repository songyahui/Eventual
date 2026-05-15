{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.Capability where
import Prelude hiding ((<>))
import Future

requestToken :: String -> Effectful RE ()
requestToken user = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "requestToken" (List [Str user]))
    , future = \_ -> finally (Atom "revokeToken" (List [Str user]))
    }

useToken :: String -> String -> Effectful RE ()
useToken user resource = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "accessResource" (List [Str user, Str resource]))
    , future = \_ -> universe
    }

-- Precondition: a token must have just been requested for this user
revokeToken :: String -> Effectful RE ()
revokeToken user = Effectful
    { ret    = ()
    , pre    = Single (Atom "requestToken" (List [Str user]))
    , post   = Single (Atom "revokeToken" (List [Str user]))
    , future = \_ -> universe
    }

escalate :: String -> Effectful RE ()
escalate role = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "escalate" (List [Str role]))
    , future = \_ -> finally (Atom "deescalate" (List [Str role]))
    }

deescalate :: String -> Effectful RE ()
deescalate role = Effectful
    { ret    = ()
    , pre    = Single (Atom "escalate" (List [Str role]))
    , post   = Single (Atom "deescalate" (List [Str role]))
    , future = \_ -> universe
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
    putStrLn $ "Future: " ++ show (normalize (evalFuture prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "properTokenUse"       properTokenUse
    printResult "safeEscalation"       safeEscalation
    printResult "tokenLeak"            tokenLeak
    printResult "privilegeLeak"        privilegeLeak
    printResult "revokeWithoutRequest" revokeWithoutRequest
