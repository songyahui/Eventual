{-# OPTIONS_GHC -i.. #-}
module Examples.CryptoSession where
import Prelude hiding ((<>))
import Future

-- Initiate an encrypted session: future = eventually finalizeSession
initSession :: String -> Effectful RE ()
initSession sid = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("initSession", [Str sid])
    , future = finally ("finalizeSession", [Str sid])
    }

-- Finalize session: discharges initSession obligation
finalizeSession :: String -> Effectful RE ()
finalizeSession sid = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("finalizeSession", [Str sid])
    , future = anything
    }

-- Generate a nonce: future = eventually consumeNonce (use-once enforcement)
generateNonce :: Int -> Effectful RE ()
generateNonce nid = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("generateNonce", [Num nid])
    , future = finally ("consumeNonce", [Num nid])
    }

-- Consume the nonce: discharges generateNonce obligation
consumeNonce :: Int -> Effectful RE ()
consumeNonce nid = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("consumeNonce", [Num nid])
    , future = anything
    }

-- Encrypt data within a session: no new obligation
encrypt :: String -> String -> Effectful RE ()
encrypt sid msg = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("encrypt", [Str sid, Str msg])
    , future = anything
    }

-- Good: session opened, nonce generated and consumed, session closed
goodHandshake :: Effectful RE ()
goodHandshake = do
    initSession "sess-1"
    generateNonce 42
    encrypt "sess-1" "hello"
    consumeNonce 42
    finalizeSession "sess-1"

-- Bad: nonce generated but never consumed (replay attack risk)
nonceLeak :: Effectful RE ()
nonceLeak = do
    initSession "sess-2"
    generateNonce 99
    encrypt "sess-2" "secret"
    finalizeSession "sess-2"

-- Bad: session never finalized (key material not wiped)
unclosedSession :: Effectful RE ()
unclosedSession = do
    initSession "sess-3"
    generateNonce 7
    consumeNonce 7
    encrypt "sess-3" "data"

printResult :: String -> Effectful RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Post:   " ++ show (post prog)
    putStrLn $ "Future: " ++ show (normalize (future prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "goodHandshake"   goodHandshake
    printResult "nonceLeak"       nonceLeak
    printResult "unclosedSession" unclosedSession
