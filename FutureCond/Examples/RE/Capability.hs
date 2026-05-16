{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.Capability where
import Prelude hiding ((<>))
import FutureCond

requestToken :: String -> FutureCond RE ()
requestToken user = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "requestToken" (List [Str user]))
    , future = \_ -> finally (Atom "revokeToken" (List [Str user]))
    }

useToken :: String -> String -> FutureCond RE ()
useToken user resource = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "accessResource" (List [Str user, Str resource]))
    , future = \_ -> universe
    }

-- Precondition: a token must have just been requested for this user
revokeToken :: String -> FutureCond RE ()
revokeToken user = FutureCond
    { ret    = ()
    , pre    = Single (Atom "requestToken" (List [Str user]))
    , post   = Single (Atom "revokeToken" (List [Str user]))
    , future = \_ -> universe
    }

escalate :: String -> FutureCond RE ()
escalate role = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "escalate" (List [Str role]))
    , future = \_ -> finally (Atom "deescalate" (List [Str role]))
    }

deescalate :: String -> FutureCond RE ()
deescalate role = FutureCond
    { ret    = ()
    , pre    = Single (Atom "escalate" (List [Str role]))
    , post   = Single (Atom "deescalate" (List [Str role]))
    , future = \_ -> universe
    }

-- Good: token acquired and immediately revoked
properTokenUse :: FutureCond RE ()
properTokenUse = do
    requestToken "alice"
    revokeToken "alice"

-- Good: privilege escalated and dropped
safeEscalation :: FutureCond RE ()
safeEscalation = do
    escalate "admin"
    deescalate "admin"

-- Bad: token never revoked — future obligation remains
tokenLeak :: FutureCond RE ()
tokenLeak = do
    requestToken "mallory"
    useToken "mallory" "/secrets"

-- Bad: privilege escalated but never dropped — future remains
privilegeLeak :: FutureCond RE ()
privilegeLeak = do
    escalate "admin"
    useToken "system" "/root"

-- Bad: revokeToken without requestToken — precondition violated
revokeWithoutRequest :: FutureCond RE ()
revokeWithoutRequest = revokeToken "eve"

printResult :: String -> FutureCond RE () -> IO ()
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
