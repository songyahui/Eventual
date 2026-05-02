{-# OPTIONS_GHC -i.. #-}
module Examples.Capability where
import Prelude hiding ((<>))
import Future

-- OAuth-style capability / token lifecycle.
-- requestToken creates a capability that MUST eventually be revoked.

-- Request an access token for a user: future = eventually revokeToken(user)
requestToken :: String -> Effectful RE ()
requestToken user = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("requestToken", [Str user])
    , future = finally ("revokeToken", [Str user])
    }

-- Use the token (e.g. access a protected resource): no new obligation
useToken :: String -> String -> Effectful RE ()
useToken user resource = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("accessResource", [Str user, Str resource])
    , future = anything
    }

-- Revoke the token: discharges the requestToken obligation
revokeToken :: String -> Effectful RE ()
revokeToken user = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("revokeToken", [Str user])
    , future = anything
    }

-- Escalate privilege: future = eventually de-escalate
escalate :: String -> Effectful RE ()
escalate role = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("escalate", [Str role])
    , future = finally ("deescalate", [Str role])
    }

deescalate :: String -> Effectful RE ()
deescalate role = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("deescalate", [Str role])
    , future = anything
    }

-- Good: token acquired, used, revoked
properTokenUse :: Effectful RE ()
properTokenUse = do
    requestToken "alice"
    useToken "alice" "/admin"
    revokeToken "alice"

-- Good: two users, both tokens revoked
multiUser :: Effectful RE ()
multiUser = do
    requestToken "alice"
    requestToken "bob"
    useToken "alice" "/reports"
    useToken "bob" "/reports"
    revokeToken "alice"
    revokeToken "bob"

-- Bad: token acquired and used but never revoked (capability leak)
tokenLeak :: Effectful RE ()
tokenLeak = do
    requestToken "mallory"
    useToken "mallory" "/secrets"

-- Bad: privilege escalated but never dropped
privilegeLeak :: Effectful RE ()
privilegeLeak = do
    escalate "admin"
    useToken "system" "/root"

printResult :: String -> Effectful RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Post:   " ++ show (post prog)
    putStrLn $ "Future: " ++ show (normalize (future prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "properTokenUse" properTokenUse
    printResult "multiUser"      multiUser
    printResult "tokenLeak"      tokenLeak
    printResult "privilegeLeak"  privilegeLeak
